defmodule Zig.CompilationModule do
  @moduledoc false

  # this module encapulates a struct which carries attributes of zig compilation
  # modules.  The fields of the struct are subject to change as the capabilites of
  # zigler are increased, or new features are added in zig.

  @enforce_keys [:name, :path]

  defstruct @enforce_keys ++ [deps: [], c: nil]

  alias Zig.Attributes

  def build_sema(module) do
    priv_dir = :code.priv_dir(:zigler)

    # main: semantic analysis file.
    main = %__MODULE__{
      name: :main,
      deps: [:analyte],
      path: Path.join(priv_dir, "beam/sema.zig")
    }

    # analyte: the code for the zigler module.

    analyte_deps = [:beam, :erl_nif, :attributes]

    analyte = %__MODULE__{
      name: :analyte,
      path: module.zig_code_path,
      deps: analyte_deps,
      c: module.c
    }

    # beam: zigler-specific beam helpers

    beam = %__MODULE__{
      name: :beam,
      deps: [:erl_nif],
      path: Path.join(priv_dir, "beam/beam.zig")
    }

    # erl_nif: stubbed erlang nif content.

    erl_nif = %__MODULE__{
      name: :erl_nif,
      path: Path.join(priv_dir, "beam/stub_erl_nif.zig")
    }

    # if the attributes were to be passed.

    attributes = %__MODULE__{
      name: :attributes,
      path: Attributes.code_path(module)
    }

    # for now.  We'll change this in a moment.
    deps = []

    [main, analyte, beam, erl_nif, attributes] ++ deps
  end

  # defp maybe_add_windows_shim(c) do
  #  # TODO: replace this with Target info
  #  case :os.type() do
  #    {_, :nt} ->
  #      :zigler
  #      |> :code.priv_dir()
  #      |> Path.join("erl_nif_win")
  #      |> then(&%{c | include_dirs: [&1 | c.include_dirs]})
  #
  #    _ ->
  #      c
  #  end
  # end
end
