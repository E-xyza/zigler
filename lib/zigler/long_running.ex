defmodule Zigler.LongRunning do

  #defp call(lst = ["beam.term" | _rest]), do: lst
  #defp call(lst = ["*e.ErlNifEnv" | _rest]), do: lst
  #defp call(lst), do: ["beam.env" | lst]

  #def functions(nif) do
  #  [
  #    {Zig.launcher(nif.name), {call(nif.params), "beam.term"}},
  #    {Zig.fetcher(nif.name), {["beam.env", "beam.term"], "beam.term"}}
  #  ]
  #end
#
  #def adapters(nif) do
  #  [
  #    Zig.launcher_fn(nif),
  #    Zig.fetch_fn(nif)
  #  ]
  #end
end

