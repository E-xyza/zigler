defmodule Zig.Nif.Marshaller do
  @moduledoc """
  renders a nif marshalling function into an elixir macro.
  """

  @enforce_keys [:type, :name, :args, :arg_exprs, :entrypoint, :return_expr, :param_error_clauses]
  defstruct @enforce_keys

  @type t :: %__MODULE__{
          type: :def | :defp,
          name: atom,
          args: [{atom, [], Elixir}],
          arg_exprs: [Macro.t()],
          entrypoint: atom,
          return_expr: Macro.t(),
          param_error_clauses: Macro.t()
        }

  @return {:return, [], Elixir}

  def render(%{
        param_marshalling_macros: nil,
        return_marshalling_macro: nil,
        param_error_macros: nil
      }),
      do: nil

  def render(nif) do
    name = nif.function.name

    return_fn = nif.return_marshalling_macro
    return_expr = if return_fn, do: return_fn.(@return), else: @return

    {args, arg_exprs_lists} =
      (nif.param_marshalling_macros || List.duplicate(nil, nif.function.arity))
      |> Enum.with_index(fn
        maybe_expr_func, index ->
          arg = {:"arg#{index}", [], Elixir}

          assignment =
            if maybe_expr_func do
              expr = maybe_expr_func.(arg, index)

              quote do
                unquote(arg) = unquote(expr)
              end
            end

          {arg, List.wrap(assignment)}
      end)
      |> Enum.unzip()

    arg_exprs = Enum.flat_map(arg_exprs_lists, & &1)

    param_error_clauses =
      case Enum.reduce(
             nif.param_error_macros || [],
             {[], 0, false},
             # TODO: break out into defp
             fn
               # skip "env"
               :env, {[], 0, false} ->
                 {[], 0, false}

               :env, _ ->
                # TODO: line number for this error.
                raise CompileError, "you may not have an env type except in the first position"

               nil, {clauses_so_far, index, any_so_far?} ->
                 {clauses_so_far, index, any_so_far?}

               clauses_fn, {clauses_so_far, index, _any_so_far} ->
                 new_clauses =
                   index
                   |> clauses_fn.()
                   |> Enum.map(fn {prefix_macro, result_macro} ->
                     quote do
                       :error, unquote(prefix_macro) -> unquote(result_macro)
                     end
                   end)

                 {clauses_so_far ++ new_clauses, index + 1, true}
             end
           ) do
        {_, _, false} ->
          nil

        {clauses, _, _} ->
          Enum.flat_map(clauses, & &1)
      end

    do_render(%__MODULE__{
      type: nif.type,
      name: name,
      args: args,
      arg_exprs: arg_exprs,
      entrypoint: nif.entrypoint,
      return_expr: return_expr,
      param_error_clauses: param_error_clauses
    })
  end

  defp do_render(r = %__MODULE__{param_error_clauses: nil}) do
    quote do
      unquote(r.type)(unquote(r.name)(unquote_splicing(r.args))) do
        unquote_splicing(r.arg_exprs)
        unquote(@return) = unquote(r.entrypoint)(unquote_splicing(r.args))
        unquote(r.return_expr)
      end
    end
  end

  defp do_render(r) do
    quote do
      unquote(r.type)(unquote(r.name)(unquote_splicing(r.args))) do
        unquote_splicing(r.arg_exprs)
        unquote(@return) = unquote(r.entrypoint)(unquote_splicing(r.args))
        unquote(r.return_expr)
      catch
        unquote(r.param_error_clauses)
      end
    end
  end
end
