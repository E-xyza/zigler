defmodule Zigler.Zig do

  @moduledoc false

  # contains all parts of the Zigler library which is involved in generating zig code.

  alias Zigler.Parser.Nif

  require EEx

  EEx.function_from_file(:def, :nif_adapter_unguarded, "assets/nif_adapter.zig.eex", [:assigns])
  EEx.function_from_file(:def, :nif_adapter_guarded, "assets/nif_adapter.guarded.zig.eex", [:assigns])
  EEx.function_from_file(:def, :nif_adapter_test, "assets/nif_adapter.test.zig.eex", [:assigns])

  @guarded_types ~w(
    u8 c_int c_long isize usize i32 i64 f16 f32 f64 bool
    []u8 [*c]u8 []i32 []i64 []f16 []f32 []f64
    beam.atom e.ErlNifPid beam.pid e.ErlNifBinary beam.binary
    e.ErlNifReference beam.ref)

  defp needs_guard?(params) do
    Enum.any?(params, &(&1 in @guarded_types || match?({:slice, _}, &1)))
  end

  @test_regex ~r/test_[0-9A-F]{32}/

  @spec nif_adapter({atom, {[String.t], String.t}}) :: iodata
  def nif_adapter({func, {params, type}}) do
    has_env = match?(["?*e.ErlNifEnv" | _], params) || match?(["beam.env" | _], params)
    cond do
      is_binary(func) && func =~ @test_regex ->
        new_func = func |> String.split(".") |> List.last |> String.to_atom
        nif_adapter_test(func: new_func, func_call: func, params: adjust_params(params))
      needs_guard?(params) ->
        nif_adapter_guarded(func: func, params: adjust_params(params), type: type, has_env: has_env)
      true ->
        nif_adapter_unguarded(func: func, params: adjust_params(params), type: type, has_env: has_env)
    end
  end

  @spec adjust_params(any) :: [any]
  def adjust_params(params) do
    Enum.reject(params, &(&1 in ["?*e.ErlNifEnv" , "beam.env"]))
  end

  @nif_header File.read!("assets/nif_header.zig")
  @spec nif_header() :: iodata
  def nif_header, do: @nif_header

  @nif_footer File.read!("assets/nif_footer.zig.eex")

  @spec nif_footer(module, list, [atom]) :: iodata
  def nif_footer(module, funcs, resources) do
    [major, minor] = :nif_version
    |> :erlang.system_info
    |> List.to_string
    |> String.split(".")
    |> Enum.map(&String.to_integer/1)

    loader = case resources do
      [] -> "null"
      _ -> "nif_load"
    end

    EEx.eval_string(@nif_footer,
      nif_module: module,
      funcs: funcs,
      nif_major: major,
      nif_minor: minor,
      nif_loader: loader)
  end

  @nif_exports File.read!("assets/nif_exports.zig.eex")

  @spec nif_exports(list) :: iodata
  def nif_exports(funcs) do
    adjusted_funcs = Enum.map(funcs, fn
      {test, spec} when is_binary(test) ->
        new_test = test |> String.split(".") |> List.last
        {new_test, spec}
      any -> any
    end)

    EEx.eval_string(@nif_exports, funcs: adjusted_funcs)
  end

  @spec nif_resources([atom]) :: iodata
  def nif_resources(list) do
    resource_defs = Enum.map(list, fn atom -> """
      var #{atom}: ?*e.ErlNifResourceType = undefined;
      fn init_#{atom}_resource_#{:erlang.phash2 atom}(env: beam.env) ?*e.ErlNifResourceType {
        return e.enif_open_resource_type(
          env,
          null,
          c"#{atom}",
          destroy_#{atom},
          @intToEnum(e.ErlNifResourceFlags, 3),
          null);
      }
      """
    end)

    initializers = Enum.map(list, fn atom -> """
    #{atom} = init_#{atom}_resource_#{:erlang.phash2 atom}(env);
    """
    end)

    module_init = """
    extern fn nif_load(env: beam.env, priv: [*c]?*c_void, load_info: beam.term) c_int {
      #{initializers}
       return 0;
    }
    """

    [resource_defs, module_init]
  end

  def getfor("beam.term", idx), do: """
    arg#{idx} = argv[#{idx}];
  """
  def getfor("e.ErlNifTerm", idx), do: """
    arg#{idx} = argv[#{idx}];
  """
  def getfor("u8", idx), do: """
    arg#{idx} = try beam.get_u8(env, argv[#{idx}]);
  """
  def getfor("c_int", idx), do: """
    arg#{idx} = try beam.get_c_int(env, argv[#{idx}]);
  """
  def getfor("c_long", idx), do: """
    arg#{idx} = try beam.get_c_long(env, argv[#{idx}]);
  """
  def getfor("usize", idx), do: """
    arg#{idx} = try beam.get_usize(env, argv[#{idx}]);
  """
  def getfor("isize", idx), do: """
    arg#{idx} = try beam.get_isize(env, argv[#{idx}]);
  """
  def getfor("i32", idx), do: """
    arg#{idx} = try beam.get_i32(env, argv[#{idx}]);
  """
  def getfor("i64", idx), do: """
    arg#{idx} = try beam.get_i64(env, argv[#{idx}]);
  """
  def getfor("f16", idx), do: """
    arg#{idx} = try beam.get_f16(env, argv[#{idx}]);
  """
  def getfor("f32", idx), do: """
    arg#{idx} = try beam.get_f32(env, argv[#{idx}]);
  """
  def getfor("f64", idx), do: """
    arg#{idx} = try beam.get_f64(env, argv[#{idx}]);
  """
  def getfor("bool", idx), do: """
    arg#{idx} = try beam.get_bool(env, argv[#{idx}]);
  """
  def getfor("beam.atom", idx), do: """
    arg#{idx} = argv[#{idx}];
    if (0 == e.enif_is_atom(env, arg#{idx})) {
      return beam.Error.FunctionClauseError;
    }
  """
  def getfor("beam.pid", idx), do: """
    arg#{idx} = try beam.get_pid(env, argv[#{idx}]);
  """
  def getfor("e.ErlNifPid", idx), do: """
    arg#{idx} = try beam.get_pid(env, argv[#{idx}]);
  """
  def getfor("[*c]u8", idx), do: """
    arg#{idx} = try beam.get_c_string(env, argv[#{idx}]);
  """
  def getfor("[]u8", idx), do: """
    arg#{idx} = try beam.get_char_slice(env, argv[#{idx}]);
  """
  def getfor("beam.binary", idx), do: """
    arg#{idx} = try beam.get_binary(env, argv[#{idx}]);
  """
  def getfor("e.ErlNifBinary", idx), do: """
    arg#{idx} = try beam.get_binary(env, argv[#{idx}]);
  """
  def getfor("[]i32", idx), do: """
    arg#{idx} = try beam.get_slice_of(i32, env, argv[#{idx}]);
    defer beam.allocator.free(arg#{idx});
  """
  def getfor("[]i64", idx), do: """
    arg#{idx} = try beam.get_slice_of(i64, env, argv[#{idx}]);
    defer beam.allocator.free(arg#{idx});
  """
  def getfor("[]f16", idx), do: """
    arg#{idx} = try beam.get_slice_of(f16, env, argv[#{idx}]);
    defer beam.allocator.free(arg#{idx});
  """
  def getfor("[]f32", idx), do: """
    arg#{idx} = try beam.get_slice_of(f32, env, argv[#{idx}]);
    defer beam.allocator.free(arg#{idx});
  """
  def getfor("[]f64", idx), do: """
    arg#{idx} = try beam.get_slice_of(f64, env, argv[#{idx}]);
    defer beam.allocator.free(arg#{idx});
  """

  def makefor("beam.atom"),    do: "return result;"
  def makefor("u8"),           do: "return beam.make_u8(env, result);"
  def makefor("c_int"),        do: "return beam.make_c_int(env, result);"
  def makefor("c_long"),       do: "return beam.make_c_long(env, result);"
  def makefor("isize"),        do: "return beam.make_c_int(env, @intCast(c_int, result));"
  def makefor("usize"),        do: "return beam.make_c_int(env, @intCast(c_int, result));"
  def makefor("i32"),          do: "return beam.make_i32(env, result);"
  def makefor("i64"),          do: "return beam.make_i64(env, result);"
  def makefor("f16"),          do: "return beam.make_f16(env, result);"
  def makefor("f32"),          do: "return beam.make_f32(env, result);"
  def makefor("f64"),          do: "return beam.make_f64(env, result);"
  def makefor("[]beam.term"),  do: "return beam.make_term_list(env, result);"
  def makefor("[]c_int"),      do: "return beam.make_c_int_list(env, result) catch { return beam.throw_enomem(env); };"
  def makefor("[]c_long"),     do: "return beam.make_c_long_list(env, result) catch { return beam.throw_enomem(env); };"
  def makefor("[]i32"),        do: "return beam.make_i32_list(env, result) catch { return beam.throw_enomem(env); };"
  def makefor("[]i64"),        do: "return beam.make_i64_list(env, result) catch { return beam.throw_enomem(env); };"
  def makefor("[]f16"),        do: "return beam.make_f16_list(env, result) catch { return beam.throw_enomem(env); };"
  def makefor("[]f32"),        do: "return beam.make_f32_list(env, result) catch { return beam.throw_enomem(env); };"
  def makefor("[]f64"),        do: "return beam.make_f64_list(env, result) catch { return beam.throw_enomem(env); };"
  def makefor("e.ErlNifTerm"), do: "return result;"
  def makefor("beam.term"),    do: "return result;"
  def makefor("bool"),         do: ~S/return if (result) e.enif_make_atom(env, c"true") else e.enif_make_atom(env, c"false");/
  def makefor("void"),         do: ~S/return e.enif_make_atom(env, c"nil");/
  def makefor("[*c]u8"),       do: """
  var result_term: e.ErlNifTerm = undefined;

  var i: usize = 0;
  while (result[i] != 0) { i += 1; }

  var bin: [*]u8 = @ptrCast([*]u8, e.enif_make_new_binary(env, i, &result_term));

  // copy over to the target:
  i = 0;
  while (result[i] != 0) { bin[i] = result[i]; i += 1;}

  return result_term;
  """
  def makefor("[]u8"), do: """
  var result_term: e.ErlNifTerm = undefined;

  var bin: [*]u8 = @ptrCast([*]u8, e.enif_make_new_binary(env, result.len, &result_term));

  for (result) | _chr, i | {
    bin[i] = result[i];
  }

  return result_term;
  """
end
