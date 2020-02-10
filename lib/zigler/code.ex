defmodule Zigler.Code do
  @moduledoc """
  all code responsible for generating zig code lives in this module.
  """

  alias Zigler.Module
  alias Zigler.Parser.Nif

  def generate(module = %Module{}) do
    header = [c_imports(module.c_includes), "\n", zig_imports(module.imports)]
    adapter = """
    fn __foo_adapter__(env: beam.nev, argc: c_int, argv: [*c] const beam.term) beam.term {
      var result: c_int = foo();
      return beam.make_c_int(env, result);
    }

    extern fn __foo_shim__(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
      var res: beam.term = __foo_adapter__(env, argc, argv) catch | err | {
        if (err == beam.Error.FunctionClauseError) {
          return beam.throw_function_clause_error(env);
        } else if (err == error.OutOfMemory) {
          return beam.throw_enomem(env);
        } else {
          return e.enif_make_badarg(env);
        }
      };
      return res;
    }
    """

    [header, "\n", module.code, "\n", adapter, "\n", footer(module)]
  end

  #############################################################################
  ## C IMPORT HANDLING

  @spec c_imports(keyword(String.t | [String.t])) :: iodata
  def c_imports(c_includes) do
    c_includes
    |> aggregate_imports
    |> Enum.map(fn
      {tgt, includes} -> """
      const #{tgt} = @cImport({
      #{c_includes includes}
      });
      """
    end)
  end

  @spec aggregate_imports(keyword(String.t | [String.t])) :: keyword([String.t])
  def aggregate_imports(c_includes) do
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
    Enum.map(imports, fn {k, v} ->
      ~s/const #{k} = @import("#{v}");\n/
    end)
  end

  #############################################################################
  ## FOOTER GENERATION

  def footer(module = %Zigler.Module{}) do
    [major, minor] = nif_major_minor()
    funcs_count = Enum.count(module.nifs)
    """
    var exported_nifs = [#{funcs_count}] e.ErlNifFunc{
    #{Enum.map(module.nifs, &nif_struct/1)}};

    export fn nif_load(env: beam.env, priv: [*c]?*c_void, load_info: beam.term) c_int {
      return 0;
    }

    const entry = e.ErlNifEntry{
      .major = #{major},
      .minor = #{minor},
      .name = c"#{module.module}",
      .num_of_funcs = #{funcs_count},
      .funcs = &(exported_nifs[0]),
      .load = nif_load,
      .reload = null,
      .upgrade = null,
      .unload = null,
      .vm_variant = c"beam.vanilla",
      .options = 1,
      .sizeof_ErlNifResourceTypeInit = 24,
      .min_erts = c"erts-#{:erlang.system_info(:version)}"
    };

    export fn nif_init() *const e.ErlNifEntry{
      return &entry;
    }
    """
  end

  @doc false
  def nif_major_minor do
    :nif_version
    |> :erlang.system_info
    |> List.to_string
    |> String.split(".")
  end

  defp nif_struct(%Nif{name: name, arity: arity}) do
    """
      e.ErlNifFunc{
        .name = c"#{name}",
        .arity = #{arity},
        .fptr = __#{name}_shim__,
        .flags = 0,
      },
    """
  end

  #############################################################################
  ## TOOLS

  # counts how many lines there are in an iolist
  defp count_lines(str) when is_binary(str) do
    str
    |> String.codepoints
    |> Enum.count(&(&1 == ?\n))
  end
  defp count_lines([a | b]), do: count_lines(a) + count_lines(b)
end
