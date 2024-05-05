defmodule Zig.Type.Manypointer do
  alias Zig.Type
  alias Zig.Type.Optional

  use Type

  import Type, only: :macros

  defstruct [:child, :repr, has_sentinel?: false]

  @type t :: %__MODULE__{
          child: Type.t(),
          repr: String.t(),
          has_sentinel?: boolean
        }

  def from_json(
        %{"child" => child, "has_sentinel" => has_sentinel?, "repr" => repr},
        module
      ) do
    %__MODULE__{
      child: Type.from_json(child, module),
      has_sentinel?: has_sentinel?,
      repr: repr
    }
  end

  def param_allowed?(pointer), do: Type.return_allowed?(pointer.child)
  def return_allowed?(pointer), do: pointer.has_sentinel? and Type.return_allowed?(pointer.child)
  def can_cleanup?(_), do: true

  def render_payload_options(_, _, _), do: Type._default_payload_options()
  def render_return(_, opts), do: Type._default_return(opts)
  def marshal_param(_, _, _, _), do: Type._default_marshal()
  def marshal_return(_, _, _), do: Type._default_marshal()

  def render_zig(type) do
    case type do
      %{has_sentinel?: false} ->
        "[*]#{Type.render_zig(type.child)}"

      %{child: ~t(u8)} ->
        "[*:0]u8"

      %{child: %Optional{}} ->
        "[*:null]#{Type.render_zig(type.child)}"
    end
  end

  # only manypointers of [*:0]u8 are allowed to be returned.
  def render_elixir_spec(%{child: ~t(u8), has_sentinel?: true}, :return, opts) do
    case opts.as do
      :list ->
        [Type.render_elixir_spec(~t(u8), :return, opts)]

      type when type in ~w(default binary)a ->
        quote do
          binary()
        end
    end
  end

  def render_elixir_spec(%{child: child, has_sentinel?: sentinel}, :param, opts)
      when not sentinel or child == ~t(u8) do
    if binary_form = binary_form(child) do
      quote context: Elixir do
        unquote([Type.render_elixir_spec(child, :param, opts)]) | unquote(binary_form)
      end
    else
      [Type.render_elixir_spec(child, :param, opts)]
    end
  end

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

  defp binary_form(%Type.Struct{packed: size}) when is_integer(size) do
    quote context: Elixir do
      <<_::_*unquote(size * 8)>>
    end
  end

  defp binary_form(_), do: nil

  def of(type, opts \\ []) do
    struct(__MODULE__, opts ++ [child: type])
  end
end
