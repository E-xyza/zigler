defmodule Zigler.Code do
  @moduledoc """
  a module that takes a parser token list and reconstitutes it as zig code.
  """

  def to_iolist(lst), do: to_iolist(lst, [])
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
  def to_iolist([list | rest], acc) when is_list(list) do
    to_iolist(rest, [acc, "(", to_iolist(list), ")"])
  end

  def to_string(parser_list) do
    parser_list
    |> to_iolist
    |> :erlang.iolist_to_binary
  end
end
