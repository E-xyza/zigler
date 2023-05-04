defmodule Zig.Nif.Marshaller do
  @doc """
  renders a nif marshalling function into an elixir macro.

  Currently the following cases are marshalled for Synchronous, DirtyIO,and
  DirtyCPU cases:

  - long integer to binary conversions (in both directions)
  - catching raises and turning them into actual erlang raises
  """

  @enforce_keys [:type, :name, :args, :arg_exprs, :entrypoint, :return_expr, :error_prongs]
  defstruct @enforce_keys

  @type t :: %__MODULE__{
          type: :def | :defp,
          name: atom,
          args: [{atom, [], Elixir}],
          arg_exprs: [Macro.t()],
          entrypoint: atom,
          return_expr: Macro.t(),
          error_prongs: Macro.t()
        }

  @return {:return, [], Elixir}

  alias Zig.Type.Error

  def render(%{
        function: %{return: return_type},
        param_marshalling_macros: nil,
        return_marshalling_macro: nil,
        param_error_macros: nil
      })
      when not is_struct(return_type, Error),
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

    error_prongs =
      List.wrap(
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
      )

    error_prongs =
      error_prongs ++
        List.wrap(
          if match?(%Error{}, nif.function.return) do
            quote do
              # TODO: add stacktrace
              :error, {:error, type, error_return_trace} ->
                new_trace = Enum.map(error_return_trace, fn %{
                  compile_unit_name: module,
                  line_info: %{line: line, file_name: file},
                  symbol_name: function
                } ->
                  # TODO: make these not do this anymore because we're natively handling binaries
                  {file, line} = __resolve("#{file}", line)
                  {:"#{module}", :"#{function}", :..., file: file, line: line}
                end)

                reraise ErlangError, [original: type], Enum.reverse(new_trace, __STACKTRACE__)
            end
          end
        )

    do_render(%__MODULE__{
      type: nif.type,
      name: name,
      args: args,
      arg_exprs: arg_exprs,
      entrypoint: nif.entrypoint,
      return_expr: return_expr,
      error_prongs: error_prongs
    })
  end

  defp do_render(r = %__MODULE__{error_prongs: []}) do
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
        unquote(r.error_prongs)
      end
    end
  end
end
