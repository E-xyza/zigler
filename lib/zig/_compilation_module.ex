defmodule Zig.CompilationModule do
  @moduledoc false

  # this module encapulates a struct which carries attributes of zig compilation
  # modules.  The fields of the struct are subject to change as the capabilites of
  # zigler are increased, or new features are added in zig.

  @enforce_keys [:name, :path]

  defstruct @enforce_keys ++ [deps: [], c: nil]

  alias Zig.Attributes
  alias Zig.Sema

  use Sema, template: "templates/sema_comp_mod.eex"
end
