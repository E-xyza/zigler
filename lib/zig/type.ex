defprotocol Zig.Type do
  alias Zig.Type.Array
  alias Zig.Type.Bool
  alias Zig.Type.Cpointer
  alias Zig.Type.Float
  alias Zig.Type.Integer
  alias Zig.Type.Manypointer
  alias Zig.Type.Optional
  alias Zig.Type.Slice
  alias Zig.Type.Struct
<<<<<<< HEAD
  alias Zig.Type.Resource

  @type t ::
          Bool.t() | Enum.t() | Float.t() | Integer.t() | Struct.t() | :env | :pid | :port | :term
=======

  @type t :: Bool.t() | Enum.t() | Float.t() | Integer.t() | Struct.t() | :env | :term
>>>>>>> 0.10.0-development

  @spec marshal_param(t, keyword) :: (Macro.t(), index :: non_neg_integer -> Macro.t()) | nil
  @doc "elixir-side type conversions that might be necessary to get an elixir parameter into a zig parameter"
  def marshal_param(type, opts)

  @spec marshal_return(t, keyword) :: (Macro.t() -> Macro.t()) | nil
  @doc "elixir-side type conversions that might be necessary to get a zig return into an elixir return"
  def marshal_return(type, opts)

  @doc "generates clauses to trap errors and convert them to expected errors"
  @spec param_errors(t, keyword) :: (integer -> [Macro.t()]) | nil
  def param_errors(type, opts)

  @doc "generates make clauses in zig"
  @spec get_result(t, keyword) :: String.t()
  def get_result(type, opts)

  @spec needs_make?(t) :: boolean
  def needs_make?(type)

<<<<<<< HEAD
  @spec missing_size?(t) :: boolean
  def missing_size?(type)

=======
>>>>>>> 0.10.0-development
  @spec to_call(t) :: String.t()
  def to_call(type)

  @spec return_allowed?(t) :: boolean
  def return_allowed?(type)

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
      %{
        "type" => "optional",
        "child" => %{"type" => "pointer", "child" => %{"name" => "stub_erl_nif.ErlNifEnv"}}
      } ->
        :env

      %{"type" => "struct", "name" => "stub_erl_nif.ERL_NIF_TERM"} ->
        :erl_nif_term

      %{
        "type" => "struct",
        "fields" => [
          %{"name" => "v", "type" => %{"name" => "stub_erl_nif.ERL_NIF_TERM"}}
        ]
      } ->
        :term

      %{"type" => "struct", "name" => "beam.term"} ->
        :term

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
<<<<<<< HEAD

      %{"type" => "resource"} ->
        Resource.from_json(json, module)

      %{"type" => "pid"} ->
        :pid

      %{"type" => "port"} ->
        :port

      %{"type" => "term"} ->
        :term
=======
>>>>>>> 0.10.0-development
    end
  end

  defmacro __using__(opts) do
    module = __CALLER__.module

    inspect? = Keyword.get(opts, :inspect?, false)

    quote bind_quoted: [inspect?: inspect?, module: module] do
      import Kernel, except: [to_string: 1]

      def marshal_param(_, _), do: nil
      def marshal_return(_, _), do: nil

      # default parameter errors handling.
      # TODO: simplify this!!
      def param_errors(type, _opts) do
        typename = "#{type}"

        fn index ->
          [
            {quote do
               {:argument_error, unquote(index), error_lines}
             end,
             quote do
               case __STACKTRACE__ do
                 [{_m, _f, a, _opts}, {m, f, _a, opts} | rest] ->
                   indentation = &["\n     ", List.duplicate("| ", &1)]

                   new_opts =
                     Keyword.merge(opts,
                       error_info: %{module: __MODULE__, function: :_format_error},
                       zigler_error: %{
                         unquote(index + 1) =>
                           error_lines
                           |> Enum.reduce({[], 0}, fn
                             :enter, {so_far, indents} ->
                               {so_far, indents + 1}

                             error_line, {so_far, indents} ->
                               error_msg =
                                 error_line
                                 |> Tuple.to_list()
                                 |> Enum.map(fn
                                   list when is_list(list) ->
                                     list

                                   string when is_binary(string) ->
                                     string

                                   {:inspect, content} ->
                                     "#{inspect(content)}"

                                   {:typename, typename} ->
                                     String.replace(typename, ".#{__MODULE__}.", "")
                                 end)
                                 |> List.wrap()
                                 |> List.insert_at(0, indentation.(indents))

                               {[error_msg | so_far], indents}
                           end)
                           |> elem(0)
                           |> Enum.reverse()
                           |> List.insert_at(0, "\n")
                           |> IO.iodata_to_binary()
                       }
                     )

                   :erlang.raise(:error, :badarg, [{m, f, a, new_opts} | rest])

                 stacktrace ->
                   # no available stacktrace info
                   :erlang.raise(:error, :badarg, stacktrace)
               end
             end}
          ]
        end
      end

      def get_result(type, opts) do
        return_type = get_in(opts, [:return, :type]) || raise "return type not found"
        "break :get_result beam.make(env, result, .{.output_as = .#{return_type}}).v;"
      end

      def needs_make?(_), do: true
<<<<<<< HEAD
      def missing_size?(_), do: false
=======
>>>>>>> 0.10.0-development

      defoverridable get_result: 2,
                     marshal_param: 2,
                     marshal_return: 2,
                     param_errors: 2,
<<<<<<< HEAD
                     needs_make?: 1,
                     missing_size?: 1
=======
                     needs_make?: 1
>>>>>>> 0.10.0-development

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
        defdelegate marshal_param(type, opts), to: module
        defdelegate marshal_return(type, opts), to: module
        defdelegate param_errors(type, opts), to: module
        defdelegate to_call(type), to: module
        defdelegate return_allowed?(type), to: module
        defdelegate get_result(type, opts), to: module
        defdelegate needs_make?(type), to: module
<<<<<<< HEAD
        defdelegate missing_size?(type), to: module
=======
>>>>>>> 0.10.0-development
        defdelegate cleanup(type, opts), to: module
      end
    end
  end
end

defimpl Zig.Type, for: Atom do
  def marshal_param(_, _), do: nil
  def marshal_return(_, _), do: nil
  def param_errors(_, _), do: nil

  def to_call(:erl_nif_term), do: "e.ErlNifTerm"
  def to_call(:term), do: "beam.term"
<<<<<<< HEAD
  def to_call(:pid), do: "beam.pid"
  def to_call(:port), do: "beam.port"
  def to_call(:void), do: "void"
  def to_call(type), do: to_string(type)

  def return_allowed?(type), do: type in ~w(term erl_nif_term pid void)a

  def get_result(:erl_nif_term, _), do: "break :get_result result;"
  def get_result(:pid, _), do: "break :get_result beam.make(env, result, .{}).v;"
=======
  def to_call(:void), do: "void"
  def to_call(type), do: to_string(type)

  def return_allowed?(type), do: type in [:term, :erl_nif_term, :void]

  def get_result(:erl_nif_term, _), do: "break :get_result result;"
>>>>>>> 0.10.0-development
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
        break :get_result beam.make(env, arg#{arg}[0..@intCast(usize, arg#{length_arg})], .{.output_as = .#{return_type}}).v;
        """
    end
  end

  def needs_make?(_), do: false
<<<<<<< HEAD
  def missing_size?(_), do: false
=======
>>>>>>> 0.10.0-development
end
