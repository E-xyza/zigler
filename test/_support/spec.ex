defmodule ZiglerTest.Spec do
  alias Zig.Nif

  def for(function, opts \\ []) do
    Nif.spec(%Nif{
      export: true,
      concurrency: :synchronous,
      type: function,
      raw: opts[:raw],
      return: opts[:return]
    })
  end
end
