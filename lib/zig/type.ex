defprotocol Zig.Type do
  alias Zig.Type.Array
  alias Zig.Type.Bool
  alias Zig.Type.Cpointer
  alias Zig.Type.Error
  alias Zig.Type.Float
  alias Zig.Type.Integer
  alias Zig.Type.Manypointer
  alias Zig.Type.Optional
  alias Zig.Type.Slice
  alias Zig.Type.Struct
  alias Zig.Type.Resource

  @type t ::
          Bool.t() | Enum.t() | Float.t() | Integer.t() | Struct.t() | :env | :pid | :port | :term

  @spec marshals_param?(t) :: boolean
  @doc "beam-side type conversions that might be necessary to get an elixir parameter into a zig parameter"
  def marshals_param?(type)

  @spec marshal_param(t, Macro.t(), non_neg_integer, :elixir | :erlang) :: Macro.t()
  def marshal_param(type, variable, index, platform)

  @spec marshals_return?(t) :: boolean
  @doc "beam-side type conversions that might be necessary to get a zig return into an elixir return"
  def marshals_return?(type)

  @spec marshal_return(t, Macro.t(), :elixir | :erlang) :: Macro.t()
  def marshal_return(type, variable, platform)

  @doc "catch prongs to correctly perform error handling, atom is a reference to function in `Zig.ErrorProng`"
  @spec error_prongs(t, :argument | :return) :: [atom]
  def error_prongs(type, context)

  @doc "generates make clauses in zig"
  @spec get_result(t, keyword) :: String.t()
  def get_result(type, opts)

  @spec needs_make?(t) :: boolean
  def needs_make?(type)

  @spec missing_size?(t) :: boolean
  def missing_size?(type)

  @spec to_call(t) :: String.t()
  def to_call(type)

  @spec return_allowed?(t) :: boolean
  def return_allowed?(type)

  @typep spec_context :: :param | :return
  @spec spec(t, spec_context, keyword) :: Macro.t()
  def spec(type, context, opts)

  import Protocol, only: []
  import Kernel

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

  @pointer_types ~w(array struct)

  def from_json(json, module) do
    case json do
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

      %{"type" => "struct"} ->
        Struct.from_json(json, module)

      %{"type" => "array"} ->
        Array.from_json(json, module)

      %{"type" => "slice"} ->
        Slice.from_json(json, module)

      %{"type" => "pointer", "child" => child = %{"type" => type}} when type in @pointer_types ->
        child
        |> __MODULE__.from_json(module)
        |> Map.replace!(:mutable, true)

      %{"type" => "manypointer"} ->
        Manypointer.from_json(json, module)

      %{"type" => "cpointer"} ->
        Cpointer.from_json(json, module)

      %{"type" => "optional"} ->
        Optional.from_json(json, module)

      %{"type" => "resource"} ->
        Resource.from_json(json, module)

      %{"type" => "error"} ->
        Error.from_json(json, module)

      %{"type" => "env"} ->
        :env

      %{"type" => "erl_nif_term"} ->
        :erl_nif_term

      %{"type" => "struct", "name" => "beam.term"} ->
        :term

      %{"type" => "pid"} ->
        :pid

      %{"type" => "port"} ->
        :port

      %{"type" => "term"} ->
        :term
    end
  end

  # convenienece function
  def spec(atom) when is_atom(atom) do
    quote context: Elixir do
      unquote(atom)()
    end
  end

  defmacro __using__(opts) do
    module = __CALLER__.module

    inspect? = Keyword.get(opts, :inspect?, false)

    quote bind_quoted: [inspect?: inspect?, module: module] do
      import Kernel, except: [to_string: 1]

      def marshals_param?(_), do: false
      def marshals_return?(_), do: false

      def marshal_param(_, _, _, _), do: raise("unreachable")
      def marshal_return(_, _, _), do: raise("unreachable")

      def error_prongs(_, :argument), do: [:argument_error_prong]
      def error_prongs(_, :return), do: []

      def get_result(type, opts) do
        return_type = Keyword.fetch!(opts, :type)
        "break :get_result beam.make(env, result, .{.output_type = .#{return_type}}).v;"
      end

      def needs_make?(_), do: true
      def missing_size?(_), do: false

      defoverridable get_result: 2,
                     marshals_param?: 1,
                     marshal_param: 4,
                     marshals_return?: 1,
                     marshal_return: 3,
                     error_prongs: 2,
                     needs_make?: 1,
                     missing_size?: 1

      defimpl String.Chars do
        defdelegate to_string(type), to: module
      end

      if inspect? do
        defimpl Inspect do
          defdelegate inspect(type, opts), to: module
        end
      else
        defimpl Inspect do
          defdelegate inspect(type, opts), to: Inspect.Any
        end
      end

      defimpl Zig.Type do
        defdelegate marshals_param?(type), to: module
        defdelegate marshal_param(type, variable, index, platform), to: module
        defdelegate marshals_return?(type), to: module
        defdelegate marshal_return(type, variable, platform), to: module
        defdelegate error_prongs(type, context), to: module

        defdelegate to_call(type), to: module
        defdelegate return_allowed?(type), to: module
        defdelegate get_result(type, opts), to: module
        defdelegate needs_make?(type), to: module
        defdelegate missing_size?(type), to: module
        defdelegate cleanup(type, opts), to: module
        defdelegate spec(type, context, opts), to: module
      end
    end
  end
end

defimpl Zig.Type, for: Atom do
  def marshals_param?(_), do: false
  def marshals_return?(_), do: false

  def marshal_param(_, _, _, _), do: raise("unreachable")
  def marshal_return(_, _, _), do: raise("unreachable")

  def error_prongs(type, :argument),
    do: List.wrap(if type in ~w(pid port)a, do: :argument_error_prong)

  def error_prongs(type, _), do: []

  def to_call(:erl_nif_term), do: "e.ErlNifTerm"
  def to_call(:term), do: "beam.term"
  def to_call(:pid), do: "beam.pid"
  def to_call(:port), do: "beam.port"
  def to_call(:void), do: "void"
  def to_call(type), do: to_string(type)

  def return_allowed?(type), do: type in ~w(term erl_nif_term pid void)a

  def get_result(:erl_nif_term, _), do: "break :get_result result;"
  def get_result(:pid, _), do: "break :get_result beam.make(env, result, .{}).v;"
  def get_result(:term, _), do: "break :get_result result.v;"

  def get_result(:void, opts) do
    case {get_in(opts, [:return, :arg]), get_in(opts, [:return, :length])} do
      {nil, _} ->
        "break :get_result beam.make(env, result, .{}).v;"

      {arg, nil} ->
        """
        _ = result;
        break :get_result beam.make(env, arg#{arg}, .{}).v;
        """

      {arg, {:arg, length_arg}} ->
        return_type =
          opts
          |> Keyword.fetch!(:return)
          |> Keyword.fetch!(:type)

        """
        _ = result;
        break :get_result beam.make(env, arg#{arg}[0..@intCast(usize, arg#{length_arg})], .{.output_type = .#{return_type}}).v;
        """
    end
  end

  def spec(:void, :return, _), do: :ok

  def spec(:pid, _, _), do: Zig.Type.spec(:pid)

  def spec(term, _, _) when term in ~w(term erl_nif_term)a, do: Zig.Type.spec(:term)

  def needs_make?(_), do: false
  def missing_size?(_), do: false
end
