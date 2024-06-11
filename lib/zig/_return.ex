defmodule Zig.Return do
  @moduledoc false

  @enforce_keys ~w[type cleanup]a
  defstruct @enforce_keys ++ [as: :default]

  alias Zig.Type

  @type type :: :binary | :integer | :default | :list | {:list, type}

  @type t :: %__MODULE__{
          type: Type.t(),
          cleanup: boolean,
          as: type
        }

  @type opts :: [:noclean | :binary | :list | {:cleanup, boolean} | {:as, type}]

  def new(raw) when raw in ~w[term erl_nif_term]a, do: %__MODULE__{type: raw, cleanup: false}

  def new(type, options) do
    struct!(__MODULE__, [type: type] ++ normalize_options(type, options))
  end

  @as ~w[binary list integer map]a
  @options ~w[as cleanup]a

  defp normalize_options(type, options) do
    options
    |> List.wrap()
    |> Enum.map(fn
      option when option in @as ->
        {:as, option}

      :noclean ->
        {:cleanup, false}

      {k, _} = kv when k in @options ->
        kv
    end)
    |> Keyword.put_new(:cleanup, Type.can_cleanup?(type))
  end
end
