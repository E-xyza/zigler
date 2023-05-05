defmodule Zig.Type.Function do
  # function gets an access behaviour so that it can be easily used in EEx
  # files.
  @behaviour Access

  defstruct [:name, :arity, :params, :return, :opts, :raw, :file, :line, :doc]

  alias Zig.Manifest
  alias Zig.Type

  @impl true
  defdelegate fetch(function, key), to: Map

  @type t :: %__MODULE__{
          name: atom(),
          arity: non_neg_integer(),
          params: [Type.t()],
          return: Type.t(),
          opts: keyword,
          doc: nil | String.t(),
          file: nil | Path.t(),
          line: nil | non_neg_integer()
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
      raise CompileError,
        description: "functions returning #{function.return} are not allowed",
        file: function.file,
        line: function.line
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
end
