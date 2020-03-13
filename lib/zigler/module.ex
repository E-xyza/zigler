defmodule Zigler.Module do
  @moduledoc """
  this struct represents all information a zigler module bound to a
  nif should have.  WIP.
  """

  @enforce_keys [:file, :module, :otp_app]

  @default_imports [builtin: "builtin", std: "std", beam: "beam.zig"]

  defstruct @enforce_keys ++ [
    zig_file:     "",
    libs:         [],
    nifs:         [],
    resources:    [],
    zig_version:  "0.5.0",
    imports:      @default_imports,
    c_includes:   [],
    include_dirs: [],
    dry_run:      false,
    code:         [],
    semver:       ["0", "0", "0"]
  ]

  @type t :: %__MODULE__{
    file:         Path.t,
    module:       module,
    otp_app:      atom,
    zig_file:     Path.t,
    libs:         [Path.t],
    nifs:         [Zigler.Parser.Function.t],
    resources:    [Zigler.Parser.Resource.t],
    zig_version:  String.t,
    imports:      keyword(Path.t),
    c_includes:   keyword(Path.t | [Path.t]),
    include_dirs: [Path.t],
    dry_run:      boolean,
    code:         iodata,
    semver:       [String.t]
  }

  # takes the zigler imports option and turns it into the imports keyword

  def imports(nil), do: @default_imports
  def imports([:defaults | rest]), do: @default_imports ++ rest
  def imports(import_list), do: import_list

end
