defmodule Zig.Parameter do
  @moduledoc false

  @enforce_keys ~w[type cleanup]a
  defstruct @enforce_keys

  alias Zig.Type

  @type t :: %__MODULE__{
          type: Type.t(),
          cleanup: boolean
        }

  @type opts :: :noclean | [:noclean | {:cleanup, boolean}]

  def new(type, options) do
    struct!(__MODULE__, [type: type] ++ normalize_options(type, options))
  end

  @options ~w[cleanup]a

  def normalize_options(type, options) do
    options
    |> List.wrap()
    |> Enum.map(fn
      :noclean -> {:cleanup, false}
      {k, _} = kv when k in @options -> kv
    end)
    |> Keyword.put_new(:cleanup, Type.can_cleanup?(type))
  end

  def render_cleanup(parameter) do
    cleanup_parameter = unless parameter.cleanup, do: ".cleanup = false,"

    ".{#{cleanup_parameter}}"
  end
end
