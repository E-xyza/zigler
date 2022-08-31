defprotocol Zig.Type do
  alias Zig.Type.Enum
  alias Zig.Type.Integer

  @type t :: Integer.t() | :env | :term

  @spec marshal_param(t) :: (Macro.t(), index :: non_neg_integer -> Macro.t())
  @doc "elixir-side type conversions that might be necessary to get an elixir parameter into a zig parameter"
  def marshal_param(type)

  @spec marshal_return(t) :: (Macro.t() -> Macro.t())
  @doc "elixir-side type conversions that might be necessary to get a zig return into an elixir return"
  def marshal_return(type)

  @doc "generates clauses to trap errors and convert them to expected errors"
  @spec param_errors(t) :: (integer -> [Macro.t()])
  def param_errors(type)

  import Kernel

  defmacro sigil_t(string, _) do
    string
    |> Code.eval_quoted([], __CALLER__)
    |> elem(0)
    |> parse
    |> Macro.escape()
  end

  def parse(string) do
    case string do
      "u" <> _ ->
        Integer.parse(string)

      "i" <> _ ->
        Integer.parse(string)

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

  def from_json(json) do
    case json do
      "?*.beam.stub_erl_nif.ErlNifEnv" ->
        :env

      ".beam.stub_erl_nif.ERL_NIF_TERM" ->
        :erl_nif_term

      ".beam.term" ->
        :term

      %{"type" => "integer"} ->
        Integer.from_json(json)

      %{"type" => "enum"} ->
        Enum.from_json(json)
    end
  end

  @spec to_zig(t) :: String.t()
  def to_zig(:env), do: "beam.env"
  def to_zig(:term), do: "beam.term"
  def to_zig(type), do: to_string(type)

  defmacro __using__(_) do
    module = __CALLER__.module

    quote do
      import Inspect.Algebra
      import Kernel, except: [to_string: 1]

      defimpl String.Chars do
        defdelegate to_string(type), to: unquote(module)
      end

      defimpl Inspect do
        defdelegate inspect(type, opts), to: unquote(module)
      end

      defimpl Zig.Type do
        defdelegate marshal_param(type), to: unquote(module)
        defdelegate marshal_return(type), to: unquote(module)
        defdelegate param_errors(type), to: unquote(module)
      end
    end
  end
end

defimpl Zig.Type, for: Atom do
  def marshal_param(:env), do: nil
  def marshal_param(:term), do: nil

  def marshal_param(type) do
    raise "#{type} should not be a call type for elixir."
  end

  def marshal_return(:term), do: nil

  def marshal_return(type) do
    raise "#{type} should not be a return type for elixir."
  end

  def param_errors(_type), do: nil
end
