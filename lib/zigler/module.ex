defmodule Zigler.Module do
  @moduledoc """
  this struct represents all information a zigler module bound to a
  nif should have.  WIP.
  """

  @enforce_keys [:file, :module, :otp_app]

  @default_imports [std: "std", e: "erl_nif.zig",  beam: "beam.zig"]

  defstruct @enforce_keys ++ [
    zig_file:     "",
    libs:         [],
    nifs:         [],
    resources:    [],
    zig_version:  Version.parse!("0.7.0+d4c167f3c"),
    imports:      @default_imports,
    c_includes:   [],
    include_dirs: [],
    dry_run:      false,
    code:         [],
    version:      Version.parse!("0.0.0"),
    test_dirs:    nil,
    target:       nil
  ]

  @type t :: %__MODULE__{
    file:         Path.t,
    module:       module,
    otp_app:      atom,
    zig_file:     Path.t,
    libs:         [Path.t],
    nifs:         [Zigler.Parser.Function.t],
    resources:    [Zigler.Parser.Resource.t],
    zig_version:  Version.t,
    imports:      keyword(Path.t),
    c_includes:   keyword(Path.t | [Path.t]),
    include_dirs: [Path.t],
    dry_run:      boolean,
    code:         iodata,
    version:      Version.t,
    test_dirs:    nil | [Path.t],
    target:       String.t
  }

  # takes the zigler imports option and turns it into the imports keyword

  def imports(nil), do: @default_imports
  def imports([:defaults | rest]), do: @default_imports ++ rest
  def imports(import_list), do: import_list

end
