defmodule Zig.Nif.Marshaller do
  @moduledoc """
  renders a nif marshalling function into an elixir macro.
  """

  @enforce_keys [:name, :args, :arg_exprs, :entrypoint, :return_expr, :param_error_clauses]
  defstruct @enforce_keys

  @type t :: %__MODULE__{
          name: atom,
          args: [{atom, [], Elixir}],
          arg_exprs: [Macro.t()],
          entrypoint: atom,
          return_expr: Macro.t(),
          param_error_clauses: Macro.t()
        }

  @return {:return, [], Elixir}

  defp maybe_apply(function, argument, orelse \\ nil) do
    if function, do: function.(argument), else: orelse
  end

  def render(%{
        param_marshalling_macros: nil,
        return_marshalling_macro: nil,
        param_error_macros: nil
      }),
      do: nil

  def render(nif) do
    name = nif.function.name

    return_expr = maybe_apply(nif.return_marshalling_macro, @return, @return)

    {args, arg_exprs_lists} =
      (nif.param_marshalling_macros || List.duplicate(nil, nif.function.arity))
      |> Enum.with_index(fn
        maybe_func, index ->
          arg = {:"arg#{index}", [], Elixir}
          expr = maybe_apply(maybe_func, arg)
          {arg, List.wrap(expr)}
      end)
      |> Enum.unzip()

    arg_exprs = Enum.flat_map(arg_exprs_lists, &(&1))

    param_error_clauses =
      case Enum.reduce(
             nif.param_error_macros || [],
             {[], 0, false},
             # TODO: break out into defp
             fn
               # skip "env"
               :env, {[], 0, false} ->
                 {[], 0, false}

               nil, {clauses_so_far, index, any_so_far?} ->
                 {[nil | clauses_so_far], index, any_so_far?}

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
      defp unquote(r.name)(unquote_splicing(r.args)) do
        unquote_splicing(r.arg_exprs)
        unquote(@return) = unquote(r.entrypoint)(unquote_splicing(r.args))
        unquote(r.return_expr)
      end
    end
  end

  defp do_render(r) do
    quote do
      defp unquote(r.name)(unquote_splicing(r.args)) do
        unquote_splicing(r.arg_exprs)
        unquote(@return) = unquote(r.entrypoint)(unquote_splicing(r.args))
        unquote(r.return_expr)
      catch
        unquote(r.param_error_clauses)
      end
    end
  end
end
