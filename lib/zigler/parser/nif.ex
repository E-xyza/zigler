defmodule Zigler.Parser.Nif do

  @moduledoc """
  This datastructure represents structured information about a single nif
  inside of a `Zigler.sigil_Z/2` block.  This is used to generate the
  `exported_nifs` variable which is an array of `ErlNifFunc` structs.  The
  following keys are implemented:

  - name: (`t:atom/0`) the function name to be bound into the module
  - arity: (`t:arity/0`) the arity of the erlang function (the zig
    function may have a different arity).
  - doc: (`t:iodata/0`) zig docstrings which should be turned into elixir docs
  - params: (`t:String.t/0`) a list of zig types which are the parameters for
    the function
  - retval: (`t:String.t/0`) the type of the return value
  - opts: (`t:keyword`) list of nif options.
    - long: true  -- if the nif should run in a separate OS thread.
    - dirty: :cpu -- if the nif should run in a dirty cpu scheduler.
    - dirty: :io  -- if the nif should run in a dirty io scheduler.
  """

  @enforce_keys [:name, :arity]

  defstruct @enforce_keys ++ [
    doc:    nil,
    params: [],
    retval: nil,
    opts:   []
  ]

  @type option ::
    {:long, boolean} |
    {:dirty, :cpu | :io}

  @type t :: %__MODULE__{
    name: String.t,
    arity: arity,
    doc:    iodata | nil,
    params: [String.t],
    retval: String.t,
    opts: [option]
  }

end
