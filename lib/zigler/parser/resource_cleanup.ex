defmodule Zigler.Parser.ResourceCleanup do

  @enforce_keys [:for]
  defstruct @enforce_keys ++ [:name, :doc]

  @beam_envs ["beam.env", "?*e.ErlNifEnv"]

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

  # validate_params/3 : raises if an invalid parameter type is sent to to the function
  @spec validate_params([String.t], Parser.t, non_neg_integer)
    :: :ok | no_return
  def validate_params([ptype, env, name], context, line) when env in @beam_envs do
    unless ptype == Atom.to_string(context.local.for) do
      raise CompileError,
        file: context.file,
        line: line,
        description: "resource cleanup function #{name} for #{context.local.for} must have second parameter be of type #{context.local.for}. (got #{ptype})"
    end
    :ok
  end
  def validate_params([_, env, name], context, line) do
    raise CompileError,
      file: context.file,
      line: line,
      description: "resource cleanup function #{name} for #{context.local.for} must have first parameter be of type `beam.env` or `?*e.ErlNifEnv`. (got #{env})"
  end
end
