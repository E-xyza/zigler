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
      path: build.zig_code_path,
      deps: [:erl_nif, :beam, :attributes] ++ Enum.map(build.extra_modules, &module_spec/1),
      c: build.c
    }
  end

  defp module_spec(%{name: name}), do: name
  defp module_spec(%{dep: dep, src_mod: src_mod, dst_mod: dst_mod}), do: {dep, {src_mod, dst_mod}}

  # default modules

  def erl_nif do
    system_include_path =
      Path.join([:code.root_dir(), "/erts-#{:erlang.system_info(:version)}", "/include"])

    erl_nif_win_path =
      :zigler
      |> :code.priv_dir()
      |> Path.join("erl_nif_win")

    %__MODULE__{
      name: :erl_nif,
      path: Builder.beam_file("erl_nif.zig"),
      c: %C{
        include_dirs: [system: system_include_path, system: erl_nif_win_path],
        link_libc: true
      }
    }
  end

  def stub_erl_nif do
    %__MODULE__{
      name: :erl_nif,
      path: Builder.beam_file("stub_erl_nif.zig")
    }
  end

  def beam do
    %__MODULE__{
      name: :beam,
      path: Builder.beam_file("beam.zig"),
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
      path: Builder.beam_file("sema.zig"),
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
