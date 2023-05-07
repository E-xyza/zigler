defmodule Zig.Type.Cpointer do
  alias Zig.Type
  alias Zig.Type.Struct
  use Type

  import Type, only: :macros

  defstruct [:child]

  @type t :: %__MODULE__{child: Type.t()}

  def from_json(%{"child" => child}, module) do
    %__MODULE__{child: Type.from_json(child, module)}
  end

  def to_string(slice), do: "[*c]#{Kernel.to_string(slice.child)}"

  def to_call(slice), do: "[*c]#{Type.to_call(slice.child)}"

  def return_allowed?(pointer) do
    case pointer.child do
      ~t(u8) -> true
      # null-terminated list of pointers.
      %__MODULE__{child: child} -> Type.return_allowed?(child)
      # NB: we assume these are single pointer returns.
      struct = %Struct{} -> Type.return_allowed?(struct)
      _ -> false
    end
  end

  def spec(%{child: child}, :params, _opts) do
    has_solo? = match?(%Type.Struct{extern: true}, child)
    child_form = Type.spec(child, :params, [])

    case {has_solo?, binary_form(child)} do
      {false, nil} ->
        quote do
          [unquote(child_form)] | nil
        end

      {true, nil} ->
        quote do
          unquote(child_form) | [unquote(child_form)] | nil
        end

      {false, binary_form} ->
        quote do
          [unquote(child_form)] | unquote(binary_form) | nil
        end
    end
  end

  def spec(%{child: ~t(u8)}, :return, opts) do
    # assumed to be a null-terminated string
    case {opts[:length], Keyword.fetch!(opts, :type)} do
      {_, :list} ->
        quote context: Elixir do
          [0..255]
        end

      {length, type} when not is_nil(length) and type in ~w(default binary)a ->
        quote context: Elixir do
          <<_::unquote(length * 8)>>
        end

      {_, type} when type in ~w(default binary)a ->
        Type.spec(:binary)
    end
  end

  def spec(%{child: child}, :return, opts) do
    case {Keyword.fetch(opts, :length), Keyword.fetch!(opts, :type)} do
      {{:ok, _}, :default} ->
        [Type.spec(child, :return, opts)]

      {{:ok, {:arg, _}}, :binary} ->
        # this is the case where the length is drawn from one of the arguments
        # to the function.
        quote do
          <<_::_*unquote(chunk_size(child))>>
        end

      {{:ok, number}, :binary} ->
        quote do
          <<_::unquote(number * chunk_size(child))>>
        end

      {:error, _} when child.__struct__ == Type.Struct ->
        Type.spec(child, :return, opts)

      {:error, _} when child == %__MODULE__{child: ~t(u8)} ->
        quote do
          [binary()]
        end

      {:error, _} when child.__struct__ == __MODULE__ ->
        [Type.spec(child.child, :return, opts)]

      {:error, _} ->
        raise "missing length not allowed"
    end
  end

  def spec(%{child: child = %__MODULE__{}}, :return, opts) do
    [Type.spec(child, :return, opts)]
  end

  defp chunk_size(%type{bits: bits}) when type in [Type.Integer, Type.Float] do
    bits
  end

  defp chunk_size(_), do: raise("invalid type for binary *c output")

  defp binary_form(~t(u8)), do: Type.spec(:binary)

  defp binary_form(%Type.Integer{bits: bits}) do
    quote context: Elixir do
      <<_::_*unquote(Type.Integer._next_power_of_two_ceil(bits))>>
    end
  end

  defp binary_form(%Type.Float{bits: bits}) do
    quote context: Elixir do
      <<_::_*unquote(bits)>>
    end
  end

  defp binary_form(_), do: nil

  def missing_size?(_), do: true

  def of(child), do: %__MODULE__{child: child}
end
