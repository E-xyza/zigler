defmodule Zigler.Parser.ResourceCleanup do
  @enforce_keys [:for]
  defstruct @enforce_keys ++ [:name, :doc]

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
end
