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
  def code_spec(["@", "nif", ["\"", nif_name, "\""], "fn", nif_name, nif_params, "[", "]", "i64", :block | rest]) do
    [{String.to_atom(nif_name), {params(nif_params), :"[]i64"}} | code_spec(rest)]
  end
  def code_spec(["@", "nif", ["\"", nif_name, "\""], "fn", nif_name, nif_params, "[", "]", "f64", :block | rest]) do
    [{String.to_atom(nif_name), {params(nif_params), :"[]f64"}} | code_spec(rest)]
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
  def params([_, ":", "[", "]", "i64"]), do: [:"[]i64"]
  def params([_, ":", "[", "]", "f64"]), do: [:"[]f64"]
  def params([_, ":", type, "," | rest]), do: [String.to_atom(type) | params(rest)]
  def params([_, ":", "?", "*", "e.ErlNifEnv", "," | rest]), do: [:"?*e.ErlNifEnv" | params(rest)]
  def params([_, ":", "elixir.env", "," | rest]), do: [:"elixir.env" | params(rest)]
  def params([_, ":", "[", "*", "c", "]", "u8", "," | rest]), do: [:"[*c]u8" | params(rest)]
  def params([_, ":", "[", "]", "u8", "," | rest]), do: [:"[]u8" | params(rest)]
  def params([_, ":", "[", "]", "i64", "," | rest]), do: [:"[]i64" | params(rest)]
  def params([_, ":", "[", "]", "f64", "," | rest]), do: [:"[]f64" | params(rest)]
  def params(_), do: raise "invalid zig syntax"

  @nif_adapter File.read!("assets/nif_adapter.zig.eex")

  @spec nif_adapter({atom, {[atom], atom}}) :: iodata
  def nif_adapter({func, {params, type}}) do
    has_env = match?([:"?*e.ErlNifEnv" | _], params) || match?([:"elixir.env" | _], params)
    EEx.eval_string(@nif_adapter, func: func, params: adjust_params(params), type: type, has_env: has_env)
  end

  def adjust_params(params) do
    Enum.reject(params, &(&1 in [:"?*e.ErlNifEnv" , :"elixir.env"]))
  end

  # TODO: move these to an "ASSEMBLER" module.

  @nif_header File.read!("assets/nif_header.zig")
  @spec nif_header() :: iodata
  def nif_header, do: @nif_header

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
  def getfor(:i64, idx), do: """
  var int#{idx}: c_int = undefined;
  res = e.enif_get_int(env, argv[#{idx}], &int#{idx});
  arg#{idx} = @intCast(i64, int#{idx});
  """
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
  def getfor(:"[]i64", idx), do: """
  var length#{idx}: c_uint = undefined;
  var head#{idx}: e.ErlNifTerm = undefined;
  var elem#{idx}: c_int = undefined;
  var list#{idx} = argv[#{idx}];
  var idx#{idx}: usize = 0;

  res = e.enif_get_list_length(env, argv[#{idx}], &length#{idx});

  // unmarshall the list using a while loop.
  if (res != 0) {

    // but first we have to allocate memory.
    arg#{idx} = elixir.allocator.alloc(i64, @intCast(usize, length#{idx}))
      catch elixir.enomem(env);

    while (idx#{idx} < length#{idx}) {
      res = e.enif_get_list_cell(env, list#{idx}, &head#{idx}, &list#{idx});
      res = e.enif_get_int(env, head#{idx}, &elem#{idx});
      arg#{idx}[idx#{idx}] = @intCast(i64, elem#{idx});
      idx#{idx} += 1;
    }

  } else {
    return e.enif_make_atom(env, c"badarg");
  }

  // free it after we're done with the entire function.
  defer elixir.allocator.free(arg#{idx});
  """
  def getfor(:"[]f64", idx), do: """
  var length#{idx}: c_uint = undefined;
  var head#{idx}: e.ErlNifTerm = undefined;
  var elem#{idx}: f64 = undefined;
  var list#{idx} = argv[#{idx}];
  var idx#{idx}: usize = 0;

  res = e.enif_get_list_length(env, argv[#{idx}], &length#{idx});

  // unmarshall the list using a while loop.
  if (res != 0) {

    // but first we have to allocate memory.
    arg#{idx} = elixir.allocator.alloc(f64, @intCast(usize, length#{idx}))
      catch elixir.enomem(env);

    while (idx#{idx} < length#{idx}) {
      res = e.enif_get_list_cell(env, list#{idx}, &head#{idx}, &list#{idx});
      res = e.enif_get_double(env, head#{idx}, &elem#{idx});
      arg#{idx}[idx#{idx}] = elem#{idx};
      idx#{idx} += 1;
    }

  } else {
    return e.enif_make_badarg(env);
  }

  // free it after we're done with the entire function.
  defer elixir.allocator.free(arg#{idx});
  """
  def getfor(:"e.ErlNifTerm", idx), do: "arg#{idx} = argv[#{idx}];"
  def getfor(:"e.ErlNifPid", idx), do: """
  res = e.enif_get_local_pid(env, argv[#{idx}], &arg#{idx});
  """

  def makefor(:"elixir.atom"), do: "return result;"
  def makefor(:c_int), do: "return e.enif_make_int(env, result);"
  def makefor(:i64), do: "return e.enif_make_int(env, @intCast(c_int, result));"
  def makefor(:f64), do: "return e.enif_make_double(env, result);"
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

  for (result) | _chr, i | {
    bin[i] = result[i];
  }

  return result_term;
  """
  def makefor(:"[]i64"), do: """
  var term_slice = elixir.allocator.alloc(e.ErlNifTerm, result.len)
    catch elixir.enomem(env);
  defer elixir.allocator.free(term_slice);

  for (term_slice) | _term, i | {
    term_slice[i] = e.enif_make_int(env, @intCast(c_int, result[i]));
  }
  var result_term = e.enif_make_list_from_array(env, term_slice.ptr, @intCast(c_uint, result.len));

  // return the term
  return result_term;
  """
  def makefor(:"[]f64"), do: """
  var term_slice = elixir.allocator.alloc(e.ErlNifTerm, result.len)
    catch elixir.enomem(env);
  defer elixir.allocator.free(term_slice);

  for (term_slice) | _term, i | {
    term_slice[i] = e.enif_make_double(env, result[i]);
  }
  var result_term = e.enif_make_list_from_array(env, term_slice.ptr, @intCast(c_uint, result.len));

  // return the term
  return result_term;
  """

  def strip_nif(code) do
    String.replace(code, ~r/\@nif\(.*\)/U, "")
  end

end
