defmodule Zigler.Zig do

  @type primitives :: :i64 | :u64 | :i8 | :u8 | :f64 | :erl_nif_env

  @spec code_spec(String.t | [zig_token]) :: keyword({[atom], atom})
  def code_spec(code) when is_binary(code), do: code_spec(tokens(code))
  def code_spec(["@", "nif", ["\"", nif_name, "\""], "fn", nif_name, nif_params, nif_type, :block | rest]) do
    [{String.to_atom(nif_name), {params(nif_params), String.to_atom(nif_type)}} | code_spec(rest)]
  end
  def code_spec(["@", "nif", ["\"", nif_name, "\""], "fn", nif_name, nif_params, "[", "]", "const", "u8", :block | rest]) do
    [{String.to_atom(nif_name), {params(nif_params), :"[]const u8"}} | code_spec(rest)]
  end
  def code_spec(["@", "nif", ["\"", nif_name, "\""], "fn", nif_name, nif_params, "[", "*", "c", "]", "u8", :block | rest]) do
    [{String.to_atom(nif_name), {params(nif_params), :"[*c]u8"}} | code_spec(rest)]
  end
  def code_spec(["@", "nif", ["\"", nif_name, "\""], "fn", nif_name, nif_params, "[", "]", "u8", :block | rest]) do
    [{String.to_atom(nif_name), {params(nif_params), :"[]u8"}} | code_spec(rest)]
  end
  def code_spec([_ | rest]), do: code_spec(rest)
  def code_spec([]), do: []

  @type zig_token :: String.t | :block | [zig_token]
  @spec tokens(String.t) :: [zig_token]
  def tokens(code) do
    tokens(code, [], "")
  end

  @spec tokens(binary, [zig_token], String.t | non_neg_integer) :: [zig_token]
  def tokens(<<a::binary-size(1)>> <> rest, tokens_list, token_so_far) do
    cond do
      is_integer(token_so_far) && a == "{" ->
        tokens(rest, tokens_list, token_so_far + 1)
      token_so_far == "" && a == "{" ->
        tokens(rest, tokens_list, 1)
      a == "{" ->
        tokens(rest, tokens_list ++ [token_so_far], 1)

      token_so_far == 1 && a == "}" ->
        tokens(rest, tokens_list ++ [:block], "")
      is_integer(token_so_far) && a == "}" ->
        tokens(rest, tokens_list, token_so_far - 1)
      a == "}" -> throw "invalid zig syntax"
      is_integer(token_so_far) ->
        tokens(rest, tokens_list, token_so_far)

      token_so_far == "" && a =~ ~r/\s/ ->
        tokens(rest, tokens_list, "")
      a =~ ~r/\s/ ->
        tokens(rest, tokens_list ++ [token_so_far], "")
      a == "(" && token_so_far == "" ->
        [bef, aft] = String.split(rest, ")", parts: 2)
        tokens(aft, tokens_list ++ [tokens(bef)], "")
      a == "(" ->
        [bef, aft] = String.split(rest, ")", parts: 2)
        tokens(aft, tokens_list ++ [token_so_far, tokens(bef)], "")
      a =~ ~r/[[:alnum:]_\.]/ ->
        tokens(rest, tokens_list, token_so_far <> a)

      token_so_far == "" ->
        tokens(rest, tokens_list ++ [a], "")
      true ->
        tokens(rest, tokens_list ++ [token_so_far, a], "")
    end
  end
  def tokens("", tokens_list, ""), do: tokens_list
  def tokens("", tokens_list, any) when is_binary(any), do: tokens_list ++ [any]
  def tokens("", tokens_list, any) when is_integer(any), do: tokens_list ++ [:block]

  @spec params([String.t]) :: [atom]
  def params([_, ":", type]), do: [String.to_atom(type)]
  def params([_, ":", "?", "*", "e.ErlNifEnv"]), do: [:"?*e.ErlNifEnv"]
  def params([_, ":", "[", "*", "c", "]", "u8"]), do: [:"[*c]u8"]
  def params([_, ":", "[", "]", "u8"]), do: [:"[]u8"]
  def params([_, ":", type, "," | rest]), do: [String.to_atom(type) | params(rest)]
  def params([_, ":", "?", "*", "e.ErlNifEnv", "," | rest]), do: [:"?*e.ErlNifEnv" | params(rest)]
  def params([_, ":", "[", "*", "c", "]", "u8", "," | rest]), do: [:"[*c]u8" | params(rest)]
  def params([_, ":", "[", "]", "u8", "," | rest]), do: [:"[]u8" | params(rest)]
  def params(_), do: raise "invalid zig syntax"

  @nif_adapter File.read!("assets/nif_adapter.zig.eex")

  @spec nif_adapter({atom, {[atom], atom}}) :: iodata
  def nif_adapter({func, {params, type}}) do
    has_env = match?([:"?*e.ErlNifEnv" | _], params)
    EEx.eval_string(@nif_adapter, func: func, params: adjust_params(params), type: type, has_env: has_env)
  end

  def adjust_params(params) do
    Enum.reject(params, &(&1 == :"?*e.ErlNifEnv"))
  end

  @nif_header File.read!("assets/nif_header.zig.eex")

  @spec nif_header(Path.t) :: iodata
  def nif_header(zig_nif_h) do
    EEx.eval_string(@nif_header, zig_nif_h: zig_nif_h)
  end

  @nif_footer File.read!("assets/nif_footer.zig.eex")

  @spec nif_footer(module, list) :: iodata
  def nif_footer(module, list) do
    EEx.eval_string(@nif_footer, nif_module: module, funcs: list)
  end

  @nif_exports File.read!("assets/nif_exports.zig.eex")

  @spec nif_exports(list) :: iodata
  def nif_exports(funcs) do
    EEx.eval_string(@nif_exports, funcs: funcs)
  end

  def getfor(:c_int, idx), do: "res = e.enif_get_int(env, argv[#{idx}], &arg#{idx});"
  def getfor(:f64, idx), do: "res = e.enif_get_double(env, argv[#{idx}], &arg#{idx});"
  def getfor(:"[*c]u8", idx), do: """
  var bin#{idx}: e.ErlNifBinary = undefined;
  res = e.enif_inspect_binary(env, argv[#{idx}], &bin#{idx});
  arg#{idx} = bin#{idx}.data;
  """
  def getfor(:"[]u8", idx), do: """
  var bin#{idx}: e.ErlNifBinary = undefined;
  res = e.enif_inspect_binary(env, argv[#{idx}], &bin#{idx});
  arg#{idx} = bin#{idx}.data[0..bin#{idx}.size];
  """
  def getfor(:"e.ErlNifTerm", idx), do: "arg#{idx} = argv[#{idx}];"
  def getfor(:"e.ErlNifPid", idx), do: """
  res = e.enif_get_local_pid(env, argv[#{idx}], &arg#{idx});
  """

  def makefor(:c_int), do: "return e.enif_make_int(env, result);"
  def makefor(:"e.ErlNifTerm"), do: "return result;"
  def makefor(:bool), do: ~S/return if (result) e.enif_make_atom(env, c"true") else e.enif_make_atom(env, c"false");/
  def makefor(:"[*c]u8"), do: """
  var result_term: e.ErlNifTerm = undefined;

  var i: usize = 0;
  while (result[i] != 0) { i += 1; }

  var bin: [*]u8 = @ptrCast([*]u8, e.enif_make_new_binary(env, i, &result_term));

  // copy over to the target:
  i = 0;
  while (result[i] != 0) { bin[i] = result[i]; i += 1;}

  return result_term;
  """
  def makefor(:"[]u8"), do: """
  var result_term: e.ErlNifTerm = undefined;

  var bin: [*]u8 = @ptrCast([*]u8, e.enif_make_new_binary(env, result.len, &result_term));
  var i: usize = 0;
  while (i < result.len) { bin[i] = result[i]; i += 1;}

  return result_term;
  """

  def strip_nif(code) do
    String.replace(code, ~r/\@nif\(.*\)/U, "")
  end

end
