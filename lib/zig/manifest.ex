defmodule Zig.Manifest do
  # TODO: spec this better
  @type t :: [{pos_integer(), {Path.t(), pos_integer()}}]

  defmacro resolver(manifest, file, def_or_defp) do
    quote do
      @__zig_manifest unquote(manifest)

      # note that this resolver has to be rebuilt here, so that zigler can be compile-time only.

      unquote(def_or_defp)(__resolve(%{file_name: file, line: line}), do: __resolve(file, line))

      defp __resolve(unquote(file), line), do: __resolve(@__zig_manifest, [], line)

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
      |> String.split("\n")
      |> Enum.with_index(1)
      |> Enum.flat_map(fn
        {"// ref " <> rest, anchor} ->
          case String.split(rest, ":") do
            ["nofile", ""] ->
              [{anchor, {"nofile", 0}}]

            [file, line] ->
              [{anchor, {Path.absname(file), String.to_integer(line)}}]
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
