defmodule Zig.Code do
  @moduledoc """
  all code responsible for generating zig code lives in this module.
  """

  alias Zig.Module
  alias Zig.Nif.{DirtyCpu, DirtyIO, Synchronous, Test, Threaded, Yielding}
  alias Zig.Nif.{Synchronous, Test, Threaded, Yielding}
  alias Zig.Parser.{Nif, Resource}

  def headers(module = %Module{}) do
    body = case module.c_includes do
      [] -> []
      includes -> c_imports(includes) ++ ["\n"]
    end
    ++ [
      zig_imports(module.imports), "\n",
    ]
  end

  def generate_main(module = %Module{}) do
    body_lines = count_lines(module.code)

    [
      module.code,
      "// ref: #{module.zig_file} line: #{body_lines + 1}\n\n",
      "// adapters for #{module.module} in #{module.file}:\n\n",
      Enum.map(module.nifs, &adapter(&1, module.module)),
      footer(module)
    ]
  end

  #############################################################################
  ## C IMPORT HANDLING

  @spec c_imports(keyword(String.t | [String.t])) :: iodata
  def c_imports(include_specs) do
    include_specs
    |> aggregate_includes
    |> Enum.map(fn
      {tgt, includes} -> """
      const #{tgt} = @cImport({
      #{c_includes includes}
      });
      """
    end)
  end

  @spec aggregate_includes(keyword(String.t | [String.t])) :: keyword([String.t])
  def aggregate_includes(c_includes) do
    c_includes
    |> Keyword.keys
    |> Enum.uniq
    |> Enum.map(fn key ->
      {key,
        c_includes
        |> Enum.filter(fn {k, _} -> k == key end)
        |> Enum.flat_map(fn
          {_, v} when is_binary(v) -> [v]
          {_, v} when is_list(v) -> v
        end)}
    end)
  end

  @spec c_includes(String.t | [String.t]) :: String.t
  defp c_includes(include) when is_binary(include), do: ~s/  @cInclude("#{include}");/
  defp c_includes(includes) when is_list(includes) do
    includes
    |> Enum.map(&c_includes/1)
    |> Enum.join("\n")
  end

  #############################################################################
  ## ZIG IMPORT HANDLING

  def zig_imports(imports) do
    Enum.map(imports, fn
      {k, {v, q}} ->
        ~s/const #{k} = @import("#{v}").#{q};\n/
      {k, v} ->
        ~s/const #{k} = @import("#{v}");\n/
    end)
  end

  #############################################################################
  ## FUNCTION ADAPTER

  def adapter(nif = %Zig.Parser.Nif{opts: opts, test: nil}, module) do
    case opts[:concurrency] do
      :threaded ->
        Threaded.zig_adapter(nif, module)
      :yielding ->
        Yielding.zig_adapter(nif, module)
      :dirty_cpu ->
        DirtyCpu.zig_adapter(nif, module)
      :dirty_io ->
        DirtyIO.zig_adapter(nif, module)
      nil ->
        Synchronous.zig_adapter(nif, module)
    end
  end
  def adapter(nif, module), do: Test.zig_adapter(nif, module)

  #############################################################################
  ## FOOTER GENERATION

  def footer(module = %Module{}) do
    [major, minor] = nif_major_minor()

    exports = """
    export var __exported_nifs__ = [_]e.ErlNifFunc{
    #{Enum.map(module.nifs, &nif_table_entries/1)}};
    """
    resource_init_defs = Enum.map(module.resources, &resource_init_definition/1)
    resource_inits = Enum.map(module.resources, &resource_initializer/1)
    resource_manager = resource_manager(module.resources)

    nif_loader = case module.resources do
      [] -> ""
      _ ->
        """
        export fn nif_load(env: beam.env, priv: [*c]?*c_void, load_info: beam.term) c_int {
        #{resource_inits}  return 0;
        }

        """
    end

    nif_load_fn = case module.resources do
      [] -> "beam.blank_load"
      _ -> "nif_load"
    end

    ["// footer for #{module.module} in #{module.file}:\n\n",
     exports, resource_init_defs, "\n", resource_manager, nif_loader, """
    const entry = e.ErlNifEntry{
      .major = #{major},
      .minor = #{minor},
      .name = "#{module.module}",
      .num_of_funcs = __exported_nifs__.len,
      .funcs = &(__exported_nifs__[0]),
      .load = #{nif_load_fn},
      .reload = beam.blank_load,     // currently unsupported
      .upgrade = beam.blank_upgrade, // currently unsupported
      .unload = beam.blank_unload,   // currently unsupported
      .vm_variant = "beam.vanilla",
      .options = 1,
      .sizeof_ErlNifResourceTypeInit = @sizeOf(e.ErlNifResourceTypeInit),
      .min_erts = "erts-#{:erlang.system_info(:version)}"
    };

    export fn nif_init() *const e.ErlNifEntry{
      return &entry;
    }
    """]
  end

  @doc false
  def nif_major_minor do
    :nif_version
    |> :erlang.system_info
    |> List.to_string
    |> String.split(".")
  end

  @doc false
  def nif_table_entries(nif = %Nif{opts: opts, test: nil}) do
    case opts[:concurrency] do
      :threaded ->
        Threaded.nif_table_entries(nif)
      :yielding ->
        Yielding.nif_table_entries(nif)
      :dirty_cpu ->
        DirtyCpu.nif_table_entries(nif)
      :dirty_io ->
        DirtyIO.nif_table_entries(nif)
      nil ->
        Synchronous.nif_table_entries(nif)
    end
  end
  def nif_table_entries(nif) do
    Test.nif_table_entries(nif)
  end

  #############################################################################
  ## RESOURCES management

  defp resource_init_definition(res = %Resource{name: original_name}) do
    name = rename(original_name)

    cleanup = if res.cleanup do
      """

        if (res) |__res__| {
          #{res.cleanup}(env, @ptrCast(*#{original_name}, @alignCast(@alignOf(*#{original_name}), __res__)));
        } else unreachable;
      """
    else
      ""
    end

    """

    var __#{name}_resource__: beam.resource_type = undefined;

    fn __init_#{name}_resource__(env: beam.env) beam.resource_type {
      return e.enif_open_resource_type(
        env,
        null,
        \"#{name}\",
        __destroy_#{name}__,
        @intToEnum(e.ErlNifResourceFlags, 3),
        null);
    }

    export fn __destroy_#{name}__(env: beam.env, res: ?*c_void) void {#{cleanup}}
    """
  end

  defp resource_initializer(%Resource{name: original_name}) do
    name = rename(original_name)
    """
      __#{name}_resource__ = __init_#{name}_resource__(env);
    """
  end

  defp resource_manager(resources) do

    resource_mapping = Enum.map(resources, &"    #{&1.name} => return __#{rename &1.name}_resource__,\n")

    case resources do
      [] -> ""
      _ ->
        """
        fn __resource_type__(comptime T: type) beam.resource_type {
          switch (T) {
        #{resource_mapping}    else => unreachable
          }
        }

        const __resource__ = struct {
          fn create(comptime T: type, env: beam.env, value: T) !beam.term {
            return beam.resource.create(T, env, __resource_type__(T), value);
          }

          fn update(comptime T: type, env: beam.env, res: beam.term, value: T) !void {
            return beam.resource.update(T, env, __resource_type__(T), res, value);
          }

          fn fetch(comptime T: type, env: beam.env, res: beam.term) !T {
            return beam.resource.fetch(T, env, __resource_type__(T), res);
          }

          fn keep(comptime T: type, env: beam.env, res: beam.term) !void {
            return beam.resource.keep(T, env, __resource_type__(T), res);
          }

          fn release(comptime T: type, env: beam.env, res: beam.term) void {
            return beam.resource.release(env, __resource_type__(T), res);
          }
        };

        """
    end
  end

  #############################################################################
  ## TOOLS

  defp rename(name) do
    strname = Atom.to_string(name)
    if String.starts_with?(strname, "__") and String.ends_with?(strname, "__") do
      strname
      |> String.trim("__")
      |> String.to_atom
    else
      name
    end
  end

  defp count_lines(iolist), do: count_lines(iolist, 0)
  defp count_lines([first | rest], count_so_far) do
    count_lines(rest, count_so_far + count_lines(first))
  end
  defp count_lines(<<?\n, rest::binary>>, count_so_far), do: count_lines(rest, count_so_far + 1)
  defp count_lines(<<_, rest::binary>>, count_so_far), do: count_lines(rest, count_so_far)
  defp count_lines(<<>>, count_so_far), do: count_so_far
  defp count_lines([], count_so_far), do: count_so_far
  defp count_lines(10, count_so_far), do: count_so_far + 1
  defp count_lines(n, count_so_far) when is_number(n), do: count_so_far
end
