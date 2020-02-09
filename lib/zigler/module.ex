defmodule Zigler.Module do
  @moduledoc """
  this struct represents all information a zigler module bound to a
  nif should have.  WIP.
  """

  @enforce_keys [:file, :module]

  defstruct @enforce_keys ++ [
    nifs:        [],
    zig_version: "0.5.0",
    imports:     [std: "std", beam: "beam.zig"],
    c_includes:  [e: "erl_nif_zig.h"],
    dry_run:     false,
    code:        []
  ]

  @type t :: %__MODULE__{
    file:        Path.t,
    module:      module,
    nifs:        [Zigler.Parser.Function.t],
    zig_version: String.t,
    imports:     keyword(Path.t),
    c_includes:  keyword(Path.t),
    dry_run:     boolean,
    code:        iodata
  }

end
