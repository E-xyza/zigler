defmodule ZiglerTest.Spec do
  alias Zig.Nif

  def for(signature, opts \\ []) do
    Nif.spec(%Nif{
      name: :my_nif,
      export: true,
      concurrency: :synchronous,
      signature: signature,
      return: opts[:return]
    })
  end
end
