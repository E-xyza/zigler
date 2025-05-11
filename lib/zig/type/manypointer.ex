defmodule Zig.Type.Manypointer do
  @moduledoc false

  alias Zig.Parameter
  alias Zig.Return
  alias Zig.Type
  alias Zig.Type.Optional

  use Type

  import Type, only: :macros

  @enforce_keys ~w[child repr const]a
  defstruct @enforce_keys ++ [has_sentinel?: false]

  @type t :: %__MODULE__{
          child: Type.t(),
          repr: String.t(),
          has_sentinel?: boolean
        }

  def from_json(
        %{"child" => child, "has_sentinel" => has_sentinel?, "repr" => repr, "is_const" => const},
        module
      ) do
    %__MODULE__{
      child: Type.from_json(child, module),
      const: const,
      has_sentinel?: has_sentinel?,
      repr: repr
    }
  end

  @impl true
  def get_allowed?(pointer), do: Type.make_allowed?(pointer.child)
  @impl true
  def make_allowed?(pointer), do: pointer.has_sentinel? and Type.make_allowed?(pointer.child)
  @impl true
  def in_out_allowed?(pointer) do
    Type.make_allowed?(pointer.child) and Type.get_allowed?(pointer.child)
  end

  @impl true
  def binary_size(pointer) do
    case Type.binary_size(pointer.child) do
      size when is_integer(size) -> {:var, size}
      {:indirect, size} -> {:var, size}
      _ -> nil
    end
  end

  @impl true
  def render_accessory_variables(type, param, prefix) do
    in_out =
      List.wrap(
        if param.in_out do
          ~s(var #{prefix}: #{render_zig(type)} = undefined;)
        end
      )

    cleanup = List.wrap(if param.cleanup, do: ~s(var @"#{prefix}-size": usize = undefined;))

    in_out ++ cleanup
  end

  @impl true
  def payload_options(_, prefix) do
    [error_info: "&error_info", size: ~s(&@"#{prefix}-size")]
  end

  @impl true
  def render_cleanup(_type, index), do: ~s(.{.cleanup = true, .size = @"arg#{index}-size"},)

  @impl true
  def marshal_param(_, variable, _, platform), do: Type._default_marshal_param(platform, variable)
  @impl true
  def marshal_return(_, variable, platform), do: Type._default_marshal_return(platform, variable)

  @impl true
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
  @impl true
  def render_elixir_spec(%{child: ~t(u8), has_sentinel?: true}, %Return{as: as} = context) do
    case as do
      :list ->
        [Type.render_elixir_spec(~t(u8), context)]

      type when type in ~w[default binary]a ->
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
