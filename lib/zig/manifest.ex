defmodule Zig.Manifest do
  @moduledoc false

  alias Zig.Command

  # TODO: spec this better
  @type t :: [{pos_integer(), {Path.t(), pos_integer()}}]

  defmacro resolver(manifest, file, def_or_defp) do
    windows_file =
      case String.replace(file, "/", "\\") do
        <<drive_letter, ":", rest::binary>> when drive_letter in ?a..?z ->
          <<drive_letter - 32, ":", rest::binary>>

        other ->
          other
      end

    quote do
      @__zig_manifest unquote(manifest)

      # note that this resolver has to be rebuilt here, so that zigler can be compile-time only.

      unquote(def_or_defp)(__resolve(%{file_name: file, line: line}), do: __resolve(file, line))
      unquote(def_or_defp)(__resolve(nil), do: {unquote(file), 0})

      defp __resolve(unquote(file), line), do: __resolve(@__zig_manifest, [], line)

      if {:win32, :nt} == :os.type() do
        @windows_file unquote(windows_file)
        defp __resolve(@windows_file, line), do: __resolve(@__zig_manifest, [], line)
      end

      defp __resolve(file, line) when is_binary(file), do: {file, line}

      defp __resolve([{anchor_line, _} = head | rest], stack, line) when anchor_line < line do
        __resolve(rest, [head | stack], line)
      end

      defp __resolve([{anchor_line, {file, _}} | _], stack, line) when anchor_line >= line do
        case List.first(stack) do
          {anchor, {file, relative_line}} ->
            {file, line - anchor + relative_line}

          _ ->
            {unquote(file), line}
        end
      end

      defp __resolve([], stack, line) do
        case List.first(stack) do
          {anchor, {file, relative_line}} ->
            {file, line - anchor + relative_line}

          _ ->
            {unquote(file), line}
        end
      end
    end
  end

  def create(module, code) do
    manifest =
      code
      |> Command.split_on_newline()
      |> Enum.with_index(1)
      |> Enum.flat_map(fn
        {"// ref " <> rest, anchor} ->
          case String.split(rest, ":") do
            ["nofile", ""] ->
              [{anchor, {"nofile", 0}}]

            list ->
              # livebooks can have colons in the path.
              line = Enum.at(list, -1)
              file = list |> Enum.slice(0..-2//1) |> Enum.join(":")
              [{anchor, {file, String.to_integer(line)}}]
          end

        _ ->
          []
      end)

    manifest_module =
      case module do
        %{language: Elixir} -> Module.concat(module.module, Manifest)
        %{language: :erlang} -> String.to_atom("#{module.module}_manifest")
      end

    [{^manifest_module, _bin}] =
      Code.compile_quoted(
        quote do
          defmodule unquote(manifest_module) do
            @moduledoc false
            require Zig.Manifest
            Zig.Manifest.resolver(unquote(manifest), unquote(module.zig_code_path), :def)
          end
        end
      )

    %{module | manifest: manifest, manifest_module: manifest_module}
  end

  def unload(module) do
    :code.soft_purge(module.manifest_module) || raise "manifest purging error"
    module
  end
end
