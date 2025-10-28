use Protoss

defprotocol Zig.Type do
  @moduledoc false

  alias Zig.Type.Array
  alias Zig.Type.Bool
  alias Zig.Type.Cpointer
  alias Zig.Type.Error
  alias Zig.Type.Float
  alias Zig.Type.Integer
  alias Zig.Type.Manypointer
  alias Zig.Type.Optional
  alias Zig.Type.Pointer
  alias Zig.Type.Slice
  alias Zig.Type.Struct
  alias Zig.Type.Resource

  @type t ::
          Array.t()
          | Bool.t()
          | Cpointer.t()
          | Error.t()
          | Zig.Type.Enum.t()
          | Float.t()
          | Integer.t()
          | ManyPointer.t()
          | Optional.t()
          | Pointer.t()
          | Slice.t()
          | Struct.t()
          | :void
          | :anyopaque
          | :env
          | :pid
          | :port
          | :term
          | :erl_nif_term
          | :erl_nif_binary
          | :erl_nif_event
          | :erl_nif_binary_pointer
          | :stacktrace

  @type json :: number | String.t() | boolean | nil | [json] | %{optional(String.t()) => json}

  @spec marshal_param(t, Macro.t(), non_neg_integer, Elixir | :erlang) :: Macro.t()
  def marshal_param(type, variable_ast, index, platform)

  @spec marshal_return(t, Macro.t(), Elixir | :erlang) :: Macro.t()
  def marshal_return(type, variable_ast, platform)

  # validations:

  @spec get_allowed?(t) :: boolean
  def get_allowed?(type)

  @spec make_allowed?(t) :: boolean
  def make_allowed?(type)

  @spec in_out_allowed?(t) :: boolean
  def in_out_allowed?(type)

  @spec binary_size(t) :: nil | non_neg_integer | {:var, non_neg_integer}
  def binary_size(type)

  # rendered zig code:
  @spec render_accessory_variables(t, term, iodata) :: iodata
  def render_accessory_variables(type, opts, prefix)

  @spec payload_options(t, String.t()) :: iodata
  def payload_options(type, prefix)

  @spec render_cleanup(t, non_neg_integer) :: iodata
  def render_cleanup(type, index)

  @spec render_zig(t) :: String.t()
  def render_zig(type)

  @spec render_elixir_spec(t, Return.t() | Param.t() | Return.type()) :: Macro.t()
  def render_elixir_spec(type, context)
after
  defmacro sigil_t({:<<>>, _, [string]}, _) do
    string
    |> parse
    |> Macro.escape()
  end

  def parse(string) do
    case string do
      "u" <> _ ->
        Integer.parse(string)

      "i" <> _ ->
        Integer.parse(string)

      "f" <> _ ->
        Float.parse(string)

      "c_uint" <> _ ->
        Integer.parse(string)

      "[]" <> rest ->
        Slice.of(parse(rest))

      "[:0]" <> rest ->
        Slice.of(parse(rest), has_sentinel?: true)

      "[*]" <> rest ->
        Manypointer.of(parse(rest))

      "[*:0]" <> rest ->
        Manypointer.of(parse(rest), has_sentinel?: true)

      "[*c]" <> rest ->
        Cpointer.of(parse(rest))

      "?" <> rest ->
        Optional.of(parse(rest))

      "[" <> maybe_array ->
        case Elixir.Integer.parse(maybe_array) do
          {count, "]" <> rest} ->
            Array.of(parse(rest), count)

          {count, ":0]" <> rest} ->
            Array.of(parse(rest), count, has_sentinel?: true)

          _ ->
            raise "unknown type #{string}"
        end

      "?*.cimport" <> rest ->
        if String.ends_with?(rest, "struct_enif_environment_t") do
          Env
        else
          unknown =
            rest
            |> String.split(".")
            |> List.last()

          raise "unknown type #{unknown}"
        end
    end
  end

  # derivative functions
  def binary_typespec(type) do
    case binary_size(type) do
      nil ->
        nil

      {:indirect, _} ->
        nil

      {:var, 1} ->
        quote context: Elixir do
          binary()
        end

      {:var, size} when size <= 32 ->
        quote context: Elixir do
          <<_::_*unquote(size * 8)>>
        end

      {:var, _} ->
        quote context: Elixir do
          binary()
        end

      size ->
        quote context: Elixir do
          <<_::unquote(size * 8)>>
        end
    end
  end

  # following two lines cause infinite loop
  # @callback from_json(json, term) :: t
  # @optional_callbacks [from_json: 2]

  def from_json(json, module) do
    case json do
      value when value in [:null, nil] ->
        # only allow during documentation sema passes
        if module do
          raise CompileError, description: "zigler encountered anytype"
        else
          :anytype
        end

      %{"type" => "unusable:" <> typename} ->
        # only allow during documentation sema passes
        if module do
          raise CompileError, description: "zigler encountered the unusable type #{typename}"
        else
          String.to_atom(typename)
        end

      %{"type" => "bool"} ->
        Bool.from_json(json)

      %{"type" => "void"} ->
        :void

      %{"type" => "integer"} ->
        Integer.from_json(json)

      %{"type" => "enum"} ->
        Zig.Type.Enum.from_json(json, module)

      %{"type" => "float"} ->
        Float.from_json(json)

      %{"type" => "resource"} ->
        Resource.from_json(json, module)

      %{"type" => "struct", "name" => "beam.term"} ->
        :term

      %{"type" => "struct"} ->
        Struct.from_json(json, module)

      %{"type" => "array"} ->
        Array.from_json(json, module)

      %{"type" => "slice"} ->
        Slice.from_json(json, module)

      %{"type" => "pointer"} = type ->
        Pointer.from_json(type, module)

      %{"type" => "optional", "child" => %{"type" => "pointer"}} = type ->
        Pointer.from_json(type, module)

      %{"type" => "manypointer"} ->
        Manypointer.from_json(json, module)

      %{"type" => "cpointer"} ->
        Cpointer.from_json(json, module)

      %{"type" => "optional"} ->
        Optional.from_json(json, module)

      %{"type" => "error"} ->
        Error.from_json(json, module)

      %{"type" => "env"} ->
        :env

      %{"type" => "erl_nif_term"} ->
        :erl_nif_term

      %{"type" => "pid"} ->
        :pid

      %{"type" => "port"} ->
        :port

      %{"type" => "term"} ->
        :term

      %{"type" => "e.ErlNifBinary"} ->
        :erl_nif_binary

      %{"type" => "e.ErlNifEvent"} ->
        :erl_nif_event

      %{"type" => "pointer", "child" => %{"type" => "e.ErlNifBinary"}} ->
        :erl_nif_binary_pointer

      %{"type" => "pointer", "child" => %{"type" => "builtin.StackTrace"}} ->
        :stacktrace

      %{"type" => "builtin.StackTrace"} ->
        :stacktrace
    end
  end

  def needs_make?(:erl_nif_term), do: false
  def needs_make?(:term), do: false
  def needs_make?(_), do: true

  # defaults

  def _default_payload_options, do: [error_info: "&error_info"]

  def _default_accessory_variables, do: []

  def _default_marshal_param(Elixir, _variable), do: []
  def _default_marshal_param(:erlang, variable), do: "#{variable}_m = #{variable},"

  def _default_marshal_return(Elixir, variable), do: variable
  def _default_marshal_return(:erlang, _variable), do: "Return"

  def _default_cleanup, do: ".{},"
end

defimpl Zig.Type, for: Atom do
  alias Zig.Type

  @impl true
  def get_allowed?(type), do: type in ~w[term erl_nif_term pid]a

  @impl true
  def make_allowed?(type), do: type in ~w[term erl_nif_term pid void]a

  @impl true
  def in_out_allowed?(_), do: false

  @impl true
  def binary_size(_), do: nil

  @impl true
  def render_zig(:term), do: "beam.term"
  def render_zig(:erl_nif_term), do: "e.erl_nif_term"
  def render_zig(:pid), do: "beam.pid"
  def render_zig(:env), do: "beam.env"
  def render_zig(:anyopaque), do: "anyopaque"
  def render_zig(atom), do: "#{atom}"

  @impl true
  def render_accessory_variables(_, _, _), do: Type._default_accessory_variables()

  @impl true
  def payload_options(:erl_nif_term, _), do: []
  def payload_options(:term, _), do: []

  def payload_options(type, _)
      when type in ~w[env stacktrace erl_nif_binary erl_nif_event erl_nif_binary_pointer]a,
      do: raise("unreachable")

  def payload_options(_, _), do: Type._default_payload_options()

  @impl true
  def render_cleanup(_, _), do: Type._default_cleanup()

  @impl true
  def render_elixir_spec(:void, %Zig.Return{}), do: :ok

  def render_elixir_spec(:pid, _) do
    quote do
      pid()
    end
  end

  @impl true
  def render_elixir_spec(term, _) when term in ~w[term erl_nif_term]a do
    quote do
      term()
    end
  end

  @impl true
  def marshal_param(_, variable, _, platform), do: Type._default_marshal_param(platform, variable)

  @impl true
  def marshal_return(_, variable, platform), do: Type._default_marshal_return(platform, variable)
end
