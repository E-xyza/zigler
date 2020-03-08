defmodule Zigler.Parser.ResourceCleanup do

  @enforce_keys [:for]
  defstruct @enforce_keys ++ [:name, :doc]

  @beam_envs ["beam.env", "?*e.ErlNifEnv"]

  alias Zigler.Parser.Resource

  # validate_arity/3: checks to make sure the arity of nif declaration matches the function
  @spec validate_arity([String.t], Parser.t, non_neg_integer)
    :: :ok | no_return
  def validate_arity(params_and_name, context, line) when length(params_and_name) != 3 do
    raise CompileError,
      file: context.file,
      line: line,
      description: "resource cleanup function #{List.last params_and_name} must have 2 parameters."
  end
  def validate_arity(_, _, _), do: :ok

  # validate_params/3 : raises if the function signarture isn't (beam.env, resource_type)
  @spec validate_params([String.t], Parser.t, non_neg_integer)
    :: :ok | no_return
  def validate_params([ptype, env, name], context, line) when env in @beam_envs do
    unless ptype == "*" <> Atom.to_string(context.local.for) do
      raise CompileError,
        file: context.file,
        line: line,
        description: "resource cleanup function #{name} for #{context.local.for} must have second parameter be of type *#{context.local.for}. (got #{ptype})"
    end
    :ok
  end
  def validate_params([_, env, name], context, line) do
    raise CompileError,
      file: context.file,
      line: line,
      description: "resource cleanup function #{name} for #{context.local.for} must have first parameter be of type `beam.env` or `?*e.ErlNifEnv`. (got #{env})"
  end

  # validate_params/3 : raises if the return value doesn't have type "void"
  @spec validate_retval([String.t], Parser.t, non_neg_integer)
    :: :ok | no_return
  def validate_retval([retval | _rest], _context, _line) when retval == "void", do: :ok
  def validate_retval([retval, _, _, name], context, line) do
    raise CompileError,
      file: context.file,
      line: line,
      description: "resource cleanup function #{name} for resource #{context.local.for} must return `void` (currently returns `#{retval}`)"
  end

  def register_function_header([_, _, _, name], context = %{local: %{for: res}}) do
    # search through global for a resource that matches our for parameter.
    # if it's found then update it.  If it's not found then merely append the
    # cleanup promise to the global parser state.
    if Enum.any?(context.global, &match?(%Resource{name: ^res}, &1)) do
      new_global = bind_cleanup(context.global, res, String.to_atom(name))
      %{context | global: new_global}
    else
      final_cleanup = %{context.local | name: String.to_atom(name)}
      %{context | global: [final_cleanup | context.global]}
    end
  end

  # no need for a null case because we know there must be at least one match.
  defp bind_cleanup([r = %Resource{name: res} | rest], res, cleanup) do
    [%{r | cleanup: cleanup} | rest]
  end
  defp bind_cleanup([_any | rest], res, cleanup) do
    [rest | bind_cleanup(rest, res, cleanup)]
  end
end
