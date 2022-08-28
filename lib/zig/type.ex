defprotocol Zig.Type do
  alias Zig.Type.Integer

  @type t :: Integer.t() | :env | :term

  @spec marshal_elixir(t) :: (Macro.t() -> Macro.t())
  @doc "elixir-side type conversions that might be necessary to get an elixir parameter into a zig parameter"
  def marshal_elixir(type)

  @spec marshal_zig(t) :: (Macro.t() -> Macro.t())
  @doc "elixir-side type conversions that might be necessary to get a zig return into an elixir return"
  def marshal_zig(type)

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
    end
  end

  @spec to_zig(t) :: String.t
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
        defdelegate marshal_elixir(type), to: unquote(module)
        defdelegate marshal_zig(type), to: unquote(module)
      end
    end
  end
end

defimpl Zig.Type, for: Atom do
  def marshal_elixir(:env), do: nil
  def marshal_elixir(:term), do: nil

  def marshal_elixir(type) do
    raise "#{type} should not be a call type for elixir."
  end

  def marshal_zig(:term), do: nil

  def marshal_zig(type) do
    raise "#{type} should not be a return type for elixir."
  end
end
