defmodule Zig.Module do
  @moduledoc """
  this struct represents all information a zigler module bound to a
  nif should have.  WIP.
  """

  @enforce_keys [:file, :module, :otp_app]

  @default_imports [std: "std", e: "erl_nif.zig",  beam: "beam.zig"]

  # libc is required to be linked in the case of *bsd based systems
  # because these operating systems do not have syscall stability and
  # ensure libc stability.
  link_libc = case :os.type() do
    {:unix, :linux} -> false
    {:unix, :freebsd} -> true
    {:unix, :darwin} -> true
    {_, :nt} -> false
  end

  defstruct @enforce_keys ++ [
    zig_file:            "",
    libs:                [],
    nifs:                [],
    resources:           [],
    zig_version:         Version.parse!("0.9.0"),
    imports:             @default_imports,
    c_includes:          [],
    include_dirs:        [],
    system_include_dirs: [],
    dry_run:             false,
    code:                [],
    version:             Version.parse!("0.0.0"),
    link_libc:           link_libc,
    test_dirs:           nil
  ]

  @type t :: %__MODULE__{
    file:                Path.t,
    module:              module,
    otp_app:             atom,
    zig_file:            Path.t,
    libs:                [Path.t],
    nifs:                [Zig.Parser.Function.t],
    resources:           [Zig.Parser.Resource.t],
    zig_version:         Version.t,
    imports:             keyword(Path.t),
    c_includes:          keyword(Path.t | [Path.t]),
    include_dirs:        [Path.t],
    system_include_dirs: [Path.t],
    link_libc:           boolean,
    dry_run:             boolean,
    code:                iodata,
    version:             Version.t,
    test_dirs:           nil | [Path.t],
  }

  # takes the zigler imports option and turns it into the imports keyword

  def imports(nil), do: @default_imports
  def imports([:defaults | rest]), do: @default_imports ++ rest
  def imports(import_list), do: import_list

end
