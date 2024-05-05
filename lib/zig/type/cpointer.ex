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

  def can_cleanup?(_), do: true

  def render_payload_options(_, _, _), do: Type._default_payload_options()
  def render_return(_, _), do: Type._default_return()

  def render_elixir_spec(%{child: child}, :param, _opts) do
    has_solo? = match?(%Type.Struct{extern: true}, child)
    child_form = Type.render_elixir_spec(child, :param, [])

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

  def render_elixir_spec(%{child: ~t(u8)}, :return, opts) do
    # assumed to be a null-terminated string
    case opts do
      %{as: :list} ->
        quote context: Elixir do
          [0..255]
        end

      %{length: length, as: type} when not is_integer(length) and type in ~w(default binary)a ->
        quote context: Elixir do
          <<_::unquote(length * 8)>>
        end

      %{as: type} when type in ~w(default binary)a ->
        quote do
          binary()
        end
    end
  end

  def render_elixir_spec(%{child: child}, :return, opts) do
    case opts do
      %{as: :default} ->
        [Type.render_elixir_spec(child, :return, opts)]

      %{as: :binary, length: {:arg, _}} ->
        # this is the case where the length is drawn from one of the arguments
        # to the function.
        quote do
          <<_::_*unquote(chunk_size(child))>>
        end

      %{as: :binary, length: length} when is_integer(length) ->
        quote do
          <<_::unquote(length * chunk_size(child))>>
        end

      _ when child.__struct__ == Type.Struct ->
        Type.render_elixir_spec(child, :return, opts)

      _ when child == %__MODULE__{child: ~t(u8)} ->
        quote do
          [binary()]
        end

      _ when child.__struct__ == __MODULE__ ->
        [Type.render_elixir_spec(child.child, :return, opts)]

      _ ->
        raise "missing length not allowed"
    end
  end

  def render_elixir_spec(%{child: child = %__MODULE__{}}, :return, opts) do
    [Type.render_elixir_spec(child, :return, opts)]
  end

  defp chunk_size(%type{bits: bits}) when type in [Type.Integer, Type.Float] do
    bits
  end

  defp chunk_size(_), do: raise("invalid type for binary *c output")

  defp binary_form(~t(u8)) do
    quote do 
      binary()
    end
  end

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

  def of(child), do: %__MODULE__{child: child}
end
