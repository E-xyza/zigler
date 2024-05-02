defmodule Zig.Return do
  @enforce_keys [:type]
  defstruct @enforce_keys ++ [:cleanup, as: :default]

  alias Zig.Type

  @type t :: %__MODULE__{
          type: Type.t(),
          cleanup: boolean,
          as: :binary | :list | :default
        }

  def new(type, options) do
    struct!(__MODULE__, [type: type] ++ normalize_options(type, options))
  end

  @ass ~w[binary list]a
  @options ~w[as cleanup]a

  defp normalize_options(type, options) do
    options
    |> Enum.map(fn
      option when option in @ass -> {:as, option}
      :noclean -> {:cleanup, false}
      {k, _} = kv when k in @options -> kv
    end)
    |> Keyword.put_new(:cleanup, Type.can_cleanup?(type))
  end

  def render(return) do
    Type.render_return(return.type, return)
  end
end
