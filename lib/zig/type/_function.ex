defmodule Zig.Type.Function do
  # function gets an access behaviour so that it can be easily used in EEx
  # files.
  @behaviour Access

  defstruct [:name, :arity, :params, :return, :opts, :raw]
  alias Zig.Type

  @impl true
  defdelegate fetch(function, key), to: Map

  @type t :: %__MODULE__{
          name: atom(),
          arity: non_neg_integer(),
          params: [Type.t()],
          return: Type.t(),
          opts: keyword
        }

  def from_json(%{"params" => params, "return" => return}, module, name, nif_opts) do
    params = Enum.map(params, &Type.from_json(&1, module))

    arity =
      case params do
        [:env | rest] -> length(rest)
        _ -> length(params)
      end

    function = %__MODULE__{
      name: name,
      arity: arity,
      params: params,
      return: Type.from_json(return, module),
      opts: nif_opts
    }

    if arity = nif_opts[:raw] do
      to_raw(function, arity)
    else
      function
    end
  end

  defp to_raw(function, arity) do
    %{function | raw: :zig, arity: arity}
  end

  def param_marshalling_macros(function) do
    list = Enum.map(function.params, &Type.marshal_param(&1, function.opts))
    if Enum.any?(list), do: list, else: nil
  end

  def return_marshalling_macro(function) do
    Type.marshal_return(function.return, function.opts)
  end

  def param_error_macros(function) do
    list = Enum.map(function.params, &Type.param_errors(&1, function.opts))
    if Enum.any?(list), do: list, else: nil
  end

  def validate!(function) do
    unless Type.return_allowed?(function.return) do
      raise CompileError, description: "functions returning #{function.return} are not allowed"
    end
  end

  @impl true
  def get_and_update(_, _, _), do: raise("you should not update a function")

  @impl true
  def pop(_, _), do: raise("you should not pop a function")

  def nif_alias_for(%{opts: opts, name: name}) do
    case opts[:alias] do
      true ->
        "#{name}_aliased_#{:erlang.phash2(name)}"

      _ ->
        name
    end
  end

  def spec(function) do
    params = Enum.map(function.params, &Type.spec(&1, []))
    return = Type.spec(function.return, [])

    quote context: Elixir do
      @spec unquote(function.name)(unquote_splicing(params)) :: unquote(return)
    end
  end
end
