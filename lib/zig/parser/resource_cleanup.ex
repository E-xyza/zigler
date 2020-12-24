defmodule Zig.Parser.ResourceCleanup do

  @moduledoc """
  resource cleanup struct
  """

  @enforce_keys [:for]
  defstruct @enforce_keys ++ [:name, :doc]

  @beam_envs ["beam.env", "?*e.ErlNifEnv"]

  alias Zig.Parser.Resource

  # validate_arity/3: checks to make sure the arity of resource cleanup declaration matches the function
  @spec validate_arity([String.t], Parser.t, non_neg_integer)
    :: :ok | no_return
  def validate_arity(args_and_name, context, line) when length(args_and_name) != 3 do
    raise SyntaxError,
      file: context.file,
      line: line + context.zig_block_line,
      description: "resource cleanup function #{List.last args_and_name} must have 2 arguments."
  end
  def validate_arity(_, _, _), do: :ok

  # validate_args/3 : raises if the function signarture isn't (beam.env, resource_type)
  @spec validate_args([String.t], Parser.t, non_neg_integer)
    :: :ok | no_return
  def validate_args([ptype, env, name], context, line) when env in @beam_envs do
    unless ptype == "*" <> Atom.to_string(context.local.for) do
      raise SyntaxError,
        file: context.file,
        line: line + context.zig_block_line,
        description: "resource cleanup function #{name} for #{context.local.for} must have second argument be of type *#{context.local.for}. (got #{ptype})"
    end
    :ok
  end
  def validate_args([_, env, name], context, line) do
    raise SyntaxError,
      file: context.file,
      line: line + context.zig_block_line,
      description: "resource cleanup function #{name} for #{context.local.for} must have first argument be of type `beam.env` or `?*e.ErlNifEnv`. (got #{env})"
  end

  # validate_args/3 : raises if the return value doesn't have type "void"
  @spec validate_retval([String.t], Parser.t, non_neg_integer)
    :: :ok | no_return
  def validate_retval([retval | _rest], _context, _line) when retval == "void", do: :ok
  def validate_retval([retval, _, _, name], context, line) do
    raise SyntaxError,
      file: context.file,
      line: line + context.zig_block_line,
      description: "resource cleanup function #{name} for resource #{context.local.for} must return `void` (currently returns `#{retval}`)"
  end

  def register_function_header([_, _, _, name], context = %{local: %{for: res}}) do
    # search through global for a resource that matches our for argument.
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
