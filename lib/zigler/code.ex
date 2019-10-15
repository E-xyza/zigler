defmodule Zigler.Code do
  @moduledoc """
  a module that takes a parser token list and reconstitutes it as zig code.
  """

  def to_iolist(tokens), do: to_iolist(tokens, [])
  def to_iolist([], acc), do: acc
  def to_iolist([:nif, _list, {:line, _} | rest], acc), do: to_iolist(rest, acc)
  def to_iolist([:import | rest], acc), do: to_iolist(rest, [acc, "@import"])
  def to_iolist([:string | rest], acc), do: to_iolist(rest, [acc, "[]u8 "])
  def to_iolist([:cstring | rest], acc), do: to_iolist(rest, [acc, "[*c]u8 "])
  def to_iolist([atom | rest], acc) when is_atom(atom) do
    to_iolist(rest, [acc, Atom.to_string(atom), " "])
  end
  def to_iolist([string | rest], acc) when is_binary(string) do
    to_iolist(rest, [acc, string, " "])
  end
  def to_iolist([{:line, n} | rest], acc) do
    to_iolist(rest, [acc, "; //line #{n}\n"])
  end
  def to_iolist([{:slice, type} | rest], acc) do
    to_iolist(rest, [acc, "[]", Atom.to_string(type), " "])
  end
  def to_iolist([{:block, n, inner} | rest], acc) do
    to_iolist(rest, [acc, "{  //line #{n}", inner, "}\n"])
  end
  def to_iolist([{:string, string} | rest], acc) do
    to_iolist(rest, [acc, ~s("#{string}")])
  end
  def to_iolist([{:docstring, _} | rest], acc), do: to_iolist(rest, acc)
  def to_iolist([list | rest], acc) when is_list(list) do
    to_iolist(rest, [acc, "(", to_iolist(list), ")"])
  end

  def to_string(tokens) do
    tokens
    |> to_iolist
    |> :erlang.iolist_to_binary
  end

  def to_spec(tokens), do: to_spec(tokens, [])
  def to_spec([], acc), do: acc
  def to_spec([:nif, [string: fn_title], {:line, _},
      :fn, fn_title, args, output, {:block, _, _} | rest], acc) when is_list(args) do

    fun = String.to_atom(fn_title)
    arg_types = args_to_typelist(args)

    to_spec(rest, acc ++ [{fun, {arg_types, output}}])
  end
  def to_spec([_ | rest], acc), do: to_spec(rest, acc)

  def to_docs(tokens), do: to_docs(tokens, [])

  def to_docs([], acc), do: acc
  def to_docs([{:docstring, str}, :nif, [string: fn_title] | rest], acc) do
    to_docs(rest, [{String.to_atom(fn_title), str} | acc])
  end
  def to_docs([_ | rest], acc), do: to_docs(rest, acc)

  def args_to_typelist([]), do: []
  def args_to_typelist([_a, b]), do: [b]
  def args_to_typelist([_a, b, "," | rest]) do
    [b] ++ args_to_typelist(rest)
  end

  def imports(tokens), do: imports(tokens, [])

  def imports([], acc), do: Enum.reverse(acc)
  def imports([:import, [string: path] | rest], acc) do
    imports(rest, [path | acc])
  end
  def imports([_ | rest], acc) do
    imports(rest, acc)
  end
end
