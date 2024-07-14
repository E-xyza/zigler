defmodule Zig.EasyC do
  @moduledoc false

  require EEx

  easy_c = Path.join(__DIR__, "templates/easy_c.zig.eex")
  EEx.function_from_file(:def, :build_from, easy_c, [:assigns])
end
