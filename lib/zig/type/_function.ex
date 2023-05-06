defmodule Zig.Type.Function do
  @moduledoc """
  module representing the zig type, as identified by performing semantic
  analysis on the zig code.
  """

  # function gets an access behaviour so that it can be easily used in EEx
  # files.
  @behaviour Access

  defstruct [:name, :arity, :params, :return]

  alias Zig.Manifest
  alias Zig.Type

  @impl true
  defdelegate fetch(function, key), to: Map

  @type t :: %__MODULE__{
          name: atom(),
          arity: non_neg_integer(),
          params: [Type.t()],
          return: Type.t()
        }

  def from_json(%{"params" => params, "return" => return, "name" => name}, module) do
    params = Enum.map(params, &Type.from_json(&1, module))

    arity =
      case params do
        [:env | rest] -> length(rest)
        _ -> length(params)
      end

    function = %__MODULE__{
      name: String.to_atom(name),
      arity: arity,
      params: params,
      return: Type.from_json(return, module)
    }
  end

  def nif_alias_for(%{opts: opts, name: name}) do
    case opts[:alias] do
      true ->
        "#{name}_aliased_#{:erlang.phash2(name)}"

      _ ->
        name
    end
  end

  def spec(function) do
    if function.opts[:raw] do
      spec_raw(function)
    else
      spec_normal(function)
    end
  end

  def spec_raw(function) do
    params =
      List.duplicate(
        quote do
          term()
        end,
        function.arity
      )

    quote context: Elixir do
      unquote(function.name)(unquote_splicing(params)) :: term()
    end
  end

  def spec_normal(function) do
    trimmed =
      case function.params do
        [:env | list] -> list
        list -> list
      end

    param_types = Enum.map(trimmed, &Type.spec(&1, :params, []))

    return_opts =
      function.opts
      |> List.wrap()
      |> Keyword.get(:return, [])

    # TODO: check for easy_c
    return =
      if arg = return_opts[:arg] do
        function.params
        |> Enum.at(arg)
        |> Type.spec(:return, return_opts)
      else
        Type.spec(function.return, :return, return_opts)
      end

    quote context: Elixir do
      unquote(function.name)(unquote_splicing(param_types)) :: unquote(return)
    end
  end

  def assign_parsed_info(function, %{code: code}, manifest) do
    if entry = Enum.find(code, &matches_name?(&1, function.name)) do
      assign_parsed_info_(function, entry, manifest)
    else
      function
    end
  end

  defp assign_parsed_info_(function, {:fn, options = %{position: position}, _}, manifest) do
    fixed_line = Manifest.resolve(manifest, position.line)

    %{function | doc: options.doc_comment, line: fixed_line}
  end

  def assign_file(function, file), do: %{function | file: Path.relative_to_cwd(file)}

  defp matches_name?({:fn, _options, params}, function) do
    Keyword.fetch!(params, :name) == function
  end

  defp matches_name?(_, _), do: false

  # Access behaviour guards
  @impl true
  def get_and_update(_, _, _), do: raise("you should not update a function")

  @impl true
  def pop(_, _), do: raise("you should not pop a function")
end
