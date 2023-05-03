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

  def spec(%{child: child}, :params, opts) do
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
    if :charlist in opts do
      quote context: Elixir do
        [0..255]
      end
    else
      Type.spec(:binary)
    end
  end

  def spec(%{child: child = %__MODULE__{}}, :return, opts) do
    [Type.spec(child, :return, opts)]
  end

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
