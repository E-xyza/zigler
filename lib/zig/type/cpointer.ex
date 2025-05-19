defmodule Zig.Type.Cpointer do
  @moduledoc false

  alias Zig.Parameter
  alias Zig.Type
  alias Zig.Type.Struct

  use Type

  import Type, only: :macros

  @enforce_keys ~w[child const]a
  defstruct @enforce_keys ++ [:sentinel]

  @type t :: %__MODULE__{
          child: Type.t(),
          const: boolean,
          sentinel: nil | 0 | :null
        }

  def from_json(%{"child" => child} = ptr, module) do
    %__MODULE__{child: Type.from_json(child, module), const: ptr["is_const"]}
  end

  @impl true
  def get_allowed?(pointer), do: Type.get_allowed?(pointer.child)

  @impl true
  def make_allowed?(pointer) do
    case pointer.child do
      ~t(u8) -> true
      # null-terminated list of pointers.
      %__MODULE__{child: child} -> Type.make_allowed?(child)
      # NB: we assume these are single pointer returns.
      struct = %Struct{} -> Type.make_allowed?(struct)
      _ -> false
    end
  end

  @impl true
  def in_out_allowed?(pointer) do
    Type.make_allowed?(pointer.child) and Type.get_allowed?(pointer.child)
  end

  @impl true
  def binary_size(_), do: nil

  @impl true
  def render_accessory_variables(type, param, prefix) do
    in_out =
      List.wrap(
        if param.in_out do
          ~s(var #{prefix}: #{render_zig(type)} = undefined;)
        end
      )

    in_out ++ [~s(var @"#{prefix}-size": usize = undefined;)]
  end

  @impl true
  def payload_options(type, prefix) do
    [error_info: "&error_info", size: ~s(&@"#{prefix}-size")] ++
      List.wrap(
        if sentinel = type.sentinel do
          {:sentinel, "@as(#{Type.render_zig(type.child)}, #{sentinel})"}
        end
      )
  end

  @impl true
  def render_cleanup(_, index) do
    ~s(.{.cleanup = true, .size = @"arg#{index}-size"},)
  end

  @impl true
  def render_zig(%{child: child}), do: "[*c]#{Type.render_zig(child)}"

  @impl true
  def render_elixir_spec(%{child: child}, %Parameter{} = parameter) do
    has_solo? = match?(%Type.Struct{extern: true}, child)
    child_form = Type.render_elixir_spec(child, parameter)

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

  def render_elixir_spec(%{child: ~t(u8)}, context) do
    # assumed to be a null-terminated string
    case context do
      %{as: :list} ->
        quote context: Elixir do
          [0..255]
        end

      %{length: length, as: type} when is_integer(length) and type in ~w[default binary]a ->
        quote context: Elixir do
          <<_::unquote(length * 8)>>
        end

      %{as: type} when type in ~w[default binary]a ->
        quote do
          binary() | nil
        end
    end
  end

  def render_elixir_spec(%{child: %__MODULE__{child: ~t(u8)}}, _) do
    # assumed to be null-terminated list of strings
    quote do
      [binary()]
    end
  end

  def render_elixir_spec(%{child: child = %__MODULE__{}}, context) do
    # assumed to be a null-terminated list of cpointers
    [Type.render_elixir_spec(child, child_context(context))]
  end

  def render_elixir_spec(%{child: child}, context) do
    case context do
      %{as: :default} ->
        [Type.render_elixir_spec(child, context)]

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
        Type.render_elixir_spec(child, child_context(context))

      _ ->
        raise "missing length not allowed"
    end
  end

  @impl true
  def marshal_param(_, variable, _, platform), do: Type._default_marshal_param(platform, variable)
  @impl true
  def marshal_return(_, variable, platform), do: Type._default_marshal_return(platform, variable)

  defp child_context(%{as: {:list, list_child_as}} = context), do: %{context | as: list_child_as}
  defp child_context(_), do: :default

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

  def of(child), do: %__MODULE__{child: child, const: false}
end
