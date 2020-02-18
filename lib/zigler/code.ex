defmodule Zigler.Code do
  @moduledoc """
  all code responsible for generating zig code lives in this module.
  """
  alias Zigler.Module
  alias Zigler.Parser.{Nif, Resource}
  alias Zigler.Code.LongRunning

  def generate_main(module = %Module{}) do
    case module.c_includes do
      [] -> []
      includes -> c_imports(includes) ++ ["\n"]
    end
    ++ [
      zig_imports(module.imports), "\n",
      module.code, "\n",
      Enum.map(module.nifs, &adapter/1),
      footer(module)
    ]
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
    [~s/const e = @import("erl_nif.zig").c;\n/,
      Enum.map(imports, fn {k, v} ->
        ~s/const #{k} = @import("#{v}");\n/
      end)]
  end

  #############################################################################
  ## ADAPTER GENERATION

  def adapter(nif = %Zigler.Parser.Nif{opts: opts}) do
    if opts[:long] do
      LongRunning.adapter(nif)
    else
      args = args(nif)

      get_clauses = get_clauses(nif)

      result_var = "__#{nif.name}_result__"
      function_call = "#{nif.name}(#{args})"

      head = "extern fn __#{nif.name}_shim__(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {"

      result = cond do
        nif.retval in ["beam.term", "e.ErlNifTerm"] ->
          """
            return #{function_call};
          }
          """
        nif.retval == "void" ->
          """
            #{function_call};

            return beam.make_nil(env);
          }
          """
        true ->
          """
            var #{result_var} = #{function_call};

            return #{make_clause nif.retval, result_var};
          }
          """
      end
      [head, "\n", get_clauses, result, "\n"]
    end
  end

  @env_types ["beam.env", "?*e.ErlNifEnv"]

  defp args(%{arity: 0, params: [p]}) when p in @env_types, do: "env"
  defp args(%{arity: 0}), do: ""
  defp args(nif = %{params: [env | rest]}) when env in @env_types do
    ["env, ", args(%{nif | params: rest})]
  end
  defp args(nif) do
    0..(nif.arity - 1)
    |> Enum.map(&"__#{nif.name}_arg#{&1}__")
    |> Enum.join(", ")
  end

  defp get_clauses(%{arity: 0}), do: ""
  defp get_clauses(%{params: params, name: name}), do: get_clauses(params, name)

  defp get_clauses([env | rest], name) when env in @env_types, do: get_clauses(rest, name)
  defp get_clauses(params, name) do
    [params
    |> Enum.with_index
    |> Enum.map(&get_clause(&1, name)),
    "\n"]
  end

  defp get_clause({term, index}, function) when term in ["beam.term", "e.ErlNifTerm"] do
    "  var __#{function}_arg#{index}__ = argv[#{index}];\n"
  end
  defp get_clause({"[]u8", index}, function) do
    ## NB: we don't deallocate strings because the BEAM returns a pointer to memory space that it owns.
    """
      var __#{function}_arg#{index}__ = beam.get_char_slice(env, argv[#{index}])
        catch return beam.raise_function_clause_error(env);
    """
  end
  defp get_clause({"[]" <> type, index}, function) do
    """
      var __#{function}_arg#{index}__ = beam.get_slice_of(#{short_name type}, env, argv[#{index}]) catch |err| switch (err) {
        error.OutOfMemory => return beam.raise_enomem(env),
        beam.Error.FunctionClauseError => return beam.raise_function_clause_error(env)
      };
      defer beam.allocator.free(__#{function}_arg#{index}__);
    """
  end
  defp get_clause({type, index}, function) do
    """
      var __#{function}_arg#{index}__ = beam.get_#{short_name type}(env, argv[#{index}])
        catch return beam.raise_function_clause_error(env);
    """
  end

  defp make_clause("[]u8", var) do
    "beam.make_slice(env, #{var})"
  end
  defp make_clause("[]" <> type, var) do
    "beam.make_#{type}_list(env, #{var}) catch return beam.raise_enomem(env)"
  end
  defp make_clause(type, var) do
    "beam.make_#{short_name type}(env, #{var})"
  end

  defp short_name("beam.pid"), do: "pid"
  defp short_name("e.ErlNifPid"), do: "pid"
  defp short_name(any), do: any

  #############################################################################
  ## FOOTER GENERATION

  ## TODO: break this up!

  def footer(module = %Module{}) do
    [major, minor] = nif_major_minor()

    funcs_count = module.nifs
    |> Enum.map(fn %{opts: opts} ->
      if opts[:long], do: 2, else: 1
    end)
    |> Enum.sum

    exports = """
    var __exported_nifs__ = [#{funcs_count}] e.ErlNifFunc{
    #{Enum.map(module.nifs, &nif_struct/1)}};
    """
    resource_init_defs = Enum.map(module.resources, &resource_init_definition/1)
    resource_inits = Enum.map(module.resources, &resource_initializer/1)

    resource_map = module.resources
    |> Enum.map(fn res -> "    #{res.name} => return __#{rename res.name}_resource__,\n" end)

    resource_mapper = case module.resources do
      [] -> ""
      _ ->
        """
        fn __resource_type__(comptime T: type) beam.resource_type {
          switch (T) {
        #{resource_map}    else => unreachable
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

          fn release(comptime T: type, env: beam.env, res: beam.term) void {
            return beam.resource.release(env, __resource_type__(T), res);
          }
        };

        """
    end

    nif_loader = case module.resources do
      [] -> ""
      _ ->
        """
        extern fn nif_load(env: beam.env, priv: [*c]?*c_void, load_info: beam.term) c_int {
        #{resource_inits}  return 0;
        }

        """
    end

    nif_load = case module.resources do
      [] -> "null"
      _ -> "nif_load"
    end

    [exports, resource_init_defs, "\n", resource_mapper, nif_loader, """
    const entry = e.ErlNifEntry{
      .major = #{major},
      .minor = #{minor},
      .name = c"#{module.module}",
      .num_of_funcs = #{funcs_count},
      .funcs = &(__exported_nifs__[0]),
      .load = #{nif_load},
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
    """]
  end

  @doc false
  def nif_major_minor do
    :nif_version
    |> :erlang.system_info
    |> List.to_string
    |> String.split(".")
  end

  defp nif_struct(%Nif{name: name, arity: arity, opts: opts}) do
    alias Zigler.Code.LongRunning

    if opts[:long] do
      """
        e.ErlNifFunc{
          .name = c"#{LongRunning.launcher name}",
          .arity = #{arity},
          .fptr = #{LongRunning.launcher name},
          .flags = 0,
        },
        e.ErlNifFunc{
          .name = c"#{LongRunning.fetcher name}",
          .arity = 1,
          .fptr = #{LongRunning.fetcher name},
          .flags = 0,
        },
      """
    else
      """
        e.ErlNifFunc{
          .name = c"#{name}",
          .arity = #{arity},
          .fptr = __#{name}_shim__,
          .flags = 0,
        },
      """
    end
  end

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
        c\"#{name}\",
        __destroy_#{name}__,
        @intToEnum(e.ErlNifResourceFlags, 3),
        null);
    }

    extern fn __destroy_#{name}__(env: beam.env, res: ?*c_void) void {#{cleanup}}
    """
  end

  defp resource_initializer(%Resource{name: original_name}) do
    name = rename(original_name)
    """
      __#{name}_resource__ = __init_#{name}_resource__(env);
    """
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

  # counts how many lines there are in an iolist
  defp count_lines(str) when is_binary(str) do
    str
    |> String.codepoints
    |> Enum.count(&(&1 == ?\n))
  end
  defp count_lines([a | b]), do: count_lines(a) + count_lines(b)
end
