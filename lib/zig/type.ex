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
  alias Zig.Type.Resource
  alias Zig.Type.Slice
  alias Zig.Type.Struct
  alias Zig.Type.Tuple

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
  @spec payload_options(t, String.t()) :: iodata
  def payload_options(type, prefix)

  @spec render_cleanup(t, non_neg_integer) :: iodata
  def render_cleanup(type, index)

  @spec needs_size?(t) :: boolean
  def needs_size?(type)

  @spec render_zig(t) :: String.t()
  def render_zig(type)

  @spec render_elixir_spec(t, Return.t() | Param.t() | Return.type()) :: Macro.t()
  def render_elixir_spec(type, context)

  @spec render_erlang_spec(t, Return.t() | Param.t() | Return.type()) :: String.t()
  def render_erlang_spec(type, context)
after
  defmacro sigil_t({:<<>>, _, [string]}, _) do
    string
    |> parse
    |> Macro.escape()
  end

  def parse("u" <> _ = string), do: Integer.parse(string)
  def parse("i" <> _ = string), do: Integer.parse(string)
  def parse("f" <> _ = string), do: Float.parse(string)
  def parse("c_uint" <> _ = string), do: Integer.parse(string)
  def parse("[]" <> rest), do: Slice.of(parse(rest))
  def parse("[:0]" <> rest), do: Slice.of(parse(rest), has_sentinel?: true)
  def parse("[*]" <> rest), do: Manypointer.of(parse(rest))
  def parse("[*:0]" <> rest), do: Manypointer.of(parse(rest), has_sentinel?: true)
  def parse("[*c]" <> rest), do: Cpointer.of(parse(rest))
  def parse("?" <> rest), do: Optional.of(parse(rest))

  def parse("[" <> maybe_array = string) do
    case Elixir.Integer.parse(maybe_array) do
      {count, "]" <> rest} -> Array.of(parse(rest), count)
      {count, ":0]" <> rest} -> Array.of(parse(rest), count, has_sentinel?: true)
      _ -> raise "unknown type #{string}"
    end
  end

  def parse("?*.cimport" <> rest) do
    if String.ends_with?(rest, "struct_enif_environment_t") do
      Env
    else
      unknown = rest |> String.split(".") |> List.last()
      raise "unknown type #{unknown}"
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

  # Handle null/nil - only allow during documentation sema passes
  def from_json(value, module) when value in [:null, nil] do
    if module, do: raise(CompileError, description: "zigler encountered anytype"), else: :anytype
  end

  # Handle unusable types - only allow during documentation sema passes
  def from_json(%{"type" => "unusable:" <> typename}, module) do
    if module do
      raise CompileError, description: "zigler encountered the unusable type #{typename}"
    else
      String.to_atom(typename)
    end
  end

  # Simple type mappings
  def from_json(%{"type" => "bool"} = json, _module), do: Bool.from_json(json)
  def from_json(%{"type" => "void"}, _module), do: :void
  def from_json(%{"type" => "integer"} = json, _module), do: Integer.from_json(json)
  def from_json(%{"type" => "enum"} = json, module), do: Zig.Type.Enum.from_json(json, module)
  def from_json(%{"type" => "float"} = json, _module), do: Float.from_json(json)
  def from_json(%{"type" => "resource"} = json, module), do: Resource.from_json(json, module)

  # Struct types
  def from_json(%{"type" => "struct", "name" => "beam.term"}, _module), do: :term
  def from_json(%{"type" => "struct", "is_tuple" => true} = json, module), do: Tuple.from_json(json, module)
  def from_json(%{"type" => "struct"} = json, module), do: Struct.from_json(json, module)

  # Collection types
  def from_json(%{"type" => "array"} = json, module), do: Array.from_json(json, module)
  def from_json(%{"type" => "slice"} = json, module), do: Slice.from_json(json, module)
  def from_json(%{"type" => "manypointer"} = json, module), do: Manypointer.from_json(json, module)
  def from_json(%{"type" => "cpointer"} = json, module), do: Cpointer.from_json(json, module)
  def from_json(%{"type" => "error"} = json, module), do: Error.from_json(json, module)

  # Pointer types - specific patterns first
  def from_json(%{"type" => "pointer", "child" => %{"type" => "e.ErlNifBinary"}}, _module), do: :erl_nif_binary_pointer
  def from_json(%{"type" => "pointer", "child" => %{"type" => "builtin.StackTrace"}}, _module), do: :stacktrace
  def from_json(%{"type" => "optional", "child" => %{"type" => "pointer"}} = json, module), do: Pointer.from_json(json, module)
  def from_json(%{"type" => "pointer"} = json, module), do: Pointer.from_json(json, module)

  # Optional (general - after specific optional patterns)
  def from_json(%{"type" => "optional"} = json, module), do: Optional.from_json(json, module)

  # Beam/NIF types
  def from_json(%{"type" => "env"}, _module), do: :env
  def from_json(%{"type" => "erl_nif_term"}, _module), do: :erl_nif_term
  def from_json(%{"type" => "pid"}, _module), do: :pid
  def from_json(%{"type" => "port"}, _module), do: :port
  def from_json(%{"type" => "term"}, _module), do: :term
  def from_json(%{"type" => "e.ErlNifBinary"}, _module), do: :erl_nif_binary
  def from_json(%{"type" => "e.ErlNifEvent"}, _module), do: :erl_nif_event
  def from_json(%{"type" => "builtin.StackTrace"}, _module), do: :stacktrace

  def needs_make?(:erl_nif_term), do: false
  def needs_make?(:term), do: false
  def needs_make?(_), do: true

  # defaults

  def _default_payload_options, do: [error_info: "&error_info"]

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
  def payload_options(:erl_nif_term, _), do: []
  def payload_options(:term, _), do: []

  def payload_options(type, _)
      when type in ~w[env stacktrace erl_nif_binary erl_nif_event erl_nif_binary_pointer]a,
      do: raise("unreachable")

  def payload_options(_, _), do: Type._default_payload_options()

  @impl true
  def render_cleanup(_, _), do: Type._default_cleanup()

  @impl true
  def needs_size?(_), do: false

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
  def render_erlang_spec(:void, %Zig.Return{}), do: "ok"
  def render_erlang_spec(:pid, _), do: "pid()"
  def render_erlang_spec(term, _) when term in ~w[term erl_nif_term]a, do: "term()"

  @impl true
  def marshal_param(_, variable, _, platform), do: Type._default_marshal_param(platform, variable)

  @impl true
  def marshal_return(_, variable, platform), do: Type._default_marshal_return(platform, variable)
end
