defmodule Zig.Type.Manypointer do
  alias Zig.Parameter
  alias Zig.Return
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

  def get_allowed?(pointer), do: Type.make_allowed?(pointer.child)
  def make_allowed?(pointer), do: pointer.has_sentinel? and Type.make_allowed?(pointer.child)
  def can_cleanup?(_), do: true

  def binary_size(pointer) do
    case Type.binary_size(pointer.child) do
      size when is_integer(size) -> {:var, size}
      {:indirect, size} -> {:var, size}
      _ -> nil
    end
  end

  def render_payload_options(_, _, _), do: Type._default_payload_options()
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
  def render_elixir_spec(%{child: ~t(u8), has_sentinel?: true}, %Return{as: as} = context) do
    case as do
      :list ->
        [Type.render_elixir_spec(~t(u8), context)]

      type when type in ~w(default binary)a ->
        quote do
          binary()
        end
    end
  end

  def render_elixir_spec(type, %Parameter{} = context) do
    if binary_form = Type.binary_typespec(type) do
      quote context: Elixir do
        unquote([Type.render_elixir_spec(type.child, context)]) | unquote(binary_form)
      end
    else
      [Type.render_elixir_spec(type.child, context)]
    end
  end

  def of(type, opts \\ []) do
    struct(__MODULE__, opts ++ [child: type])
  end
end
