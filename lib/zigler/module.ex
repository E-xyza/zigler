defmodule Zigler.Module do
  @moduledoc """
  this struct represents all information a zigler module bound to a
  nif should have.  WIP.
  """

  @enforce_keys [:file, :module, :app]

  defstruct @enforce_keys ++ [
    nifs:        [],
    resources:   [],
    zig_version: "0.5.0",
    imports:     [builtin: "builtin", std: "std", beam: "beam.zig"],
    c_includes:  [],
    dry_run:     false,
    code:        [],
    semver:      ["0", "0", "0"]
  ]

  @type t :: %__MODULE__{
    file:        Path.t,
    module:      module,
    app:         atom,
    nifs:        [Zigler.Parser.Function.t],
    resources:   [Zigler.Parser.Resource.t],
    zig_version: String.t,
    imports:     keyword(Path.t),
    c_includes:  keyword(Path.t),
    dry_run:     boolean,
    code:        iodata,
    semver:      [String.t]
  }

end
