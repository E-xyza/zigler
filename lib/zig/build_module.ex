defmodule Zig.BuildModule do
  @moduledoc false

  # this module encapulates a struct which carries attributes of extra zig modules
  # to be added.  The fields of the struct are subject to change as the capabilites of
  # zigler are increased, or new features are added in zig.

  @enforce_keys [:name, :path]

  defstruct @enforce_keys ++
              [
                :c,
                deps: [],
                root?: false,
                error_tracing: nil
              ]

  alias Zig.Builder
  alias Zig.C

  use Builder, template: "templates/build_extra_mod.zig.eex"

  def from_beam_module(build) do
    %__MODULE__{
      name: :nif,
      # Use just the filename - will be joined with module_root in template
      path: Path.basename(build.zig_code_path),
      deps: [:erl_nif, :beam, :attributes] ++ Enum.map(build.extra_modules, &module_spec/1),
      c: build.c
    }
  end

  defp module_spec(%{name: name}), do: name
  defp module_spec(%{dep: dep, src_mod: src_mod, dst_mod: dst_mod}), do: {dep, {src_mod, dst_mod}}

  # default modules

  def erl_nif do
    %__MODULE__{
      name: :erl_nif,
      path: "beam/erl_nif.zig",
      c: %C{
        # Include dirs are now handled via erts_include and erl_nif_win_path -D flags
        include_dirs: [],
        link_libc: true
      }
    }
  end

  def stub_erl_nif do
    %__MODULE__{
      name: :erl_nif,
      path: "beam/stub_erl_nif.zig"
    }
  end

  def beam do
    %__MODULE__{
      name: :beam,
      path: "beam/beam.zig",
      deps: [:erl_nif]
    }
  end

  def attributes do
    %__MODULE__{
      name: :attributes,
      path: "attributes.zig"
    }
  end

  def sema do
    %__MODULE__{
      name: :sema,
      path: "beam/sema.zig",
      deps: [:nif],
      root?: true
    }
  end

  def nif_shim do
    %__MODULE__{
      name: :nif_shim,
      path: "module.zig",
      deps: [:beam, :erl_nif, :nif],
      root?: true
    }
  end
end
