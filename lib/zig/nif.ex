defmodule Zig.Nif do
  defstruct [:type, :concurrency, :name, :function]

  alias Zig.Nif.DirtyCpu
  alias Zig.Nif.DirtyIo
  alias Zig.Nif.Synchronous
  alias Zig.Nif.Threaded
  alias Zig.Nif.Yielding
  alias Zig.Type.Function

  @type t :: %__MODULE__{
    type: :def | :defp,
    concurrency: Synchronous | Threaded | Yielding | DirtyCpu | DirtyIo,
    name: atom(),
    function: Function
  }
end
