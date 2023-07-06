defmodule Zig.Typespec do
  @moduledoc """
  handles creating typespecs for zig Nif functions
  """

  alias Zig.Parser.Nif

  @type_for %{
    "void" => :ok,
    "u8" =>
      quote context: Elixir do
        0..255
      end,
    "u16" =>
      quote context: Elixir do
        0..0xFFFF
      end,
    "u32" =>
      quote context: Elixir do
        0..0xFFFF_FFFF
      end,
    "u64" =>
      quote context: Elixir do
        non_neg_integer
      end,
    "i32" =>
      quote context: Elixir do
        -2_147_483_648..2_147_483_647
      end,
    "i64" =>
      quote context: Elixir do
        integer
      end,
    "c_uint" =>
      quote context: Elixir do
        non_neg_integer
      end,
    "c_int" =>
      quote context: Elixir do
        integer
      end,
    "c_ulong" =>
      quote context: Elixir do
        non_neg_integer
      end,
    "c_long" =>
      quote context: Elixir do
        integer
      end,
    "usize" =>
      quote context: Elixir do
        non_neg_integer
      end,
    "isize" =>
      quote context: Elixir do
        integer
      end,
    "f16" =>
      quote context: Elixir do
        float
      end,
    "f32" =>
      quote context: Elixir do
        float
      end,
    "f64" =>
      quote context: Elixir do
        float
      end,
    "bool" =>
      quote context: Elixir do
        boolean
      end,
    "beam.term" =>
      quote context: Elixir do
        term
      end,
    "e.ErlNifTerm" =>
      quote context: Elixir do
        term
      end,
    "beam.pid" =>
      quote context: Elixir do
        pid
      end,
    "e.ErlNifPid" =>
      quote context: Elixir do
        pid
      end,
    "beam.atom" =>
      quote context: Elixir do
        atom
      end,
    "[]u8" =>
      quote context: Elixir do
        binary
      end
  }

  @doc false
  def from_nif(nif = %Nif{}) do
    {nif.name, nif.arity}

    argument_types =
      Enum.flat_map(nif.args, fn
        "beam.env" ->
          []

        "?*e.ErlNifEnv" ->
          []

        arg ->
          [type_for(arg)]
      end)

    return_type = type_for(nif.retval)

    if System.version() >= "1.14" do
      {:@, [context: Elixir, imports: [{1, Kernel}]],
       [
         {:spec, [context: Elixir],
          [
            {:"::", [],
             [
               {nif.name, [], argument_types},
               return_type
             ]}
          ]}
       ]}
    else
      {:@, [context: Elixir, import: Kernel],
       [
         {:spec, [context: Elixir, import: Kernel],
          [
            {:"::", [],
             [
               {nif.name, [], argument_types},
               return_type
             ]}
          ]}
       ]}
    end
  end

  defp type_for("[]u8"), do: @type_for["[]u8"]
  defp type_for("[]" <> type), do: [type_for(type)]

  defp type_for(type) do
    @type_for[type]
  end
end
