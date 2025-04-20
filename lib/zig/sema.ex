defmodule Zig.Sema do
  @moduledoc false
  require EEx
  alias Zig.Attributes
  alias Zig.Module
  alias Zig.Nif
  alias Zig.Parameter
  alias Zig.Return
  alias Zig.Type
  alias Zig.Type.Error
  alias Zig.Type.Function
  alias Zig.Type.Integer
  alias Zig.Type.Manypointer
  alias Zig.Type.Pointer

  require Logger

  @enforce_keys [:functions, :types, :decls, :callbacks]
  defstruct @enforce_keys

  @type t :: %__MODULE__{
          functions: [Function.t()],
          types: keyword(Type.t()),
          decls: keyword(Type.t()),
          callbacks: [Function.t()]
        }

  # PHASE 1:  SEMA EXECUTION

  @spec run_sema!(Module.t()) :: Module.t()
  # performs the first stage of semantic analysis:
  # actually executing the zig command to obtaitest/mark_as_impl_test.exsn the semantic analysis of the
  # desired file.
  def run_sema!(module) do
    module.zig_code_path
    |> Zig.Command.run_sema!(attribs_file: Attributes.code_path(module), c: module.c)
    |> Jason.decode!()
    |> tap(&maybe_dump(&1, module))
    |> reject_ignored(module)
    |> reject_allocators(module)
    |> reject_error_interpreters(module)
    |> assign_callbacks(module)
    |> integrate_sema(module)
    |> then(&Map.replace!(module, :sema, &1))
  rescue
    e in Zig.CompileError ->
      Logger.error("sema error: #{Exception.message(e)}")
      reraise Zig.CompileError.resolve(e, module), __STACKTRACE__
  end

  def run_sema_doc(file) do
    file
    |> Zig.Command.run_sema_doc!()
    |> Jason.decode!()
    |> integrate_sema(%{module: nil})
  end

  defp assign_callbacks(sema, module) do
    sema
    |> Map.put("callbacks", [])
    |> then(fn sema -> Enum.reduce(module.callbacks, sema, &move_callback(&1, &2, module)) end)
  end

  defp move_callback(
         {type, name},
         %{"functions" => functions, "callbacks" => callbacks} = sema,
         module
       ) do
    {new_functions, new_callback} = find_remove(functions, [], "#{name}", type, module)
    %{sema | "functions" => new_functions, "callbacks" => [{type, new_callback} | callbacks]}
  end

  defp find_remove([%{"name" => name} = callback | rest], so_far, name, _type, _module),
    do: {Enum.reverse(so_far, rest), callback}

  defp find_remove([other | rest], so_far, name, type, module),
    do: find_remove(rest, [other | so_far], name, type, module)

  defp find_remove([], so_far, _name, _type, _module), do: {so_far, nil}

  # removes "ignored", "allocators" and "error_interpreter" functions from the semantic analysis.
  defp reject_ignored(json, module) do
    ignored = Enum.map(module.ignore, &"#{&1}")

    Map.update!(json, "functions", fn
      functions ->
        Enum.reject(functions, &(&1["name"] in ignored))
    end)
  end

  defp reject_allocators(json, module) do
    nifs =
      case module.nifs do
        {:auto, list} -> list
        list -> list
      end

    allocators =
      nifs
      |> Enum.flat_map(fn {_name, opts} -> List.wrap(opts[:allocator]) end)
      |> Enum.map(&"#{&1}")

    Map.update!(json, "functions", fn
      functions ->
        Enum.reject(functions, &(&1["name"] in allocators))
    end)
  end

  defp reject_error_interpreters(json, module) do
    nifs =
      case module.nifs do
        {:auto, list} -> list
        list -> list
      end

    # try to find a list of all functions that are opts >> return >> error
    # but then also axe out any functions that are also explicitly specified in the nifs list
    # then convert into a list of strings
    error_functions =
      nifs
      |> Enum.flat_map(fn {_, opts} -> error_fun(opts) end)
      |> Enum.reject(&Keyword.has_key?(nifs, &1))
      |> Enum.map(&"#{&1}")

    Map.update!(json, "functions", fn
      functions ->
        Enum.reject(functions, &(&1["name"] in error_functions))
    end)
  end

  defp error_fun(opts) do
    opts
    |> Keyword.get(:return, [])
    |> List.wrap()
    |> Enum.find_value(fn
      {:error, fun} -> fun
      _ -> nil
    end)
    |> List.wrap()
  end

  defp integrate_sema(
         %{
           "functions" => functions,
           "types" => types,
           "decls" => decls
         } = sema,
         module
       ) do
    callbacks = List.wrap(sema["callbacks"])

    %__MODULE__{
      functions: Enum.map(functions, &Function.from_json(&1, module.module)),
      types: Enum.map(types, &type_from_json(&1, module.module)),
      decls: Enum.map(decls, &const_from_json/1),
      callbacks:
        Enum.map(callbacks, fn
          {type, json} -> {type, json && Function.from_json(json, module.module)}
        end)
    }
  end

  defp type_from_json(%{"name" => name, "type" => type}, module) do
    %{name: String.to_atom(name), type: Type.from_json(type, module)}
  end

  defp const_from_json(%{"name" => name, "type" => type}) do
    %{name: String.to_atom(name), type: String.to_atom(type)}
  end

  defp maybe_dump(sema_json, module) do
    if module.dump_sema do
      sema_json_pretty = Jason.encode!(sema_json, pretty: true)
      IO.puts([IO.ANSI.yellow(), sema_json_pretty, IO.ANSI.reset()])
    end
  end

  @spec analyze_file!(Module.t()) :: Module.t()
  # updates the per-function options to include the semantically understood type
  # information.  Also strips "auto" from the nif information to provide a finalized
  # keyword list of functions with their options.
  def analyze_file!(%{sema: %{functions: sema_functions, types: _types}} = module) do
    module.nifs |> dbg(limit: 25)
    # `nifs` option could either be {:auto, [nifs]} which means that the full
    # list of functions should be derived from the semantic analysis, determining
    # which functions have `pub` declaration, with certain functions optionally
    # having their specification overloaded.
    #
    # it could also be just a list of functions with their specifications, in
    # which case those are the *only* functions that will be included.

    # check for invalid callbacks
    check_invalid_callbacks!(module)

    nifs =
      case module.nifs do
        {:auto, specified_nifs} ->
          # make sure that all of the specified functions exist in sema.
          Enum.each(specified_nifs, fn {name, nif_opts} ->
            expected_name = Keyword.get(nif_opts, :alias, name)

            if not Enum.any?(sema_functions, &(&1.name == expected_name)) do
              needed_msg = if nif_opts[:alias], do: " (needed by nif #{name})"

              raise CompileError,
                description:
                  "public function named `#{expected_name}`#{needed_msg} not found in semantic analysis of module.",
                file: module.file,
                line: module.line
            end
          end)

          Enum.map(sema_functions, fn sema ->
            sema
            |> make_nif(module)
            |> maybe_merge(specified_nifs, module)
          end)

        specified_nifs when is_list(specified_nifs) ->
          Enum.map(specified_nifs, fn {name, nif_opts} ->
            # TODO: remove
            match?(%Nif{}, nif_opts) or
              raise "nif option must be a nif struct (got: #{inspect(nif_opts)})"

            expected_name = Keyword.get(nif_opts, :alias, name)

            if sema_function = Enum.find(sema_functions, &(&1.name == expected_name)) do
              sema_function
              |> make_nif(module)
              |> maybe_merge(specified_nifs, module)
            else
              needed_msg = if nif_opts[:alias], do: " (needed by nif #{name})"

              raise CompileError,
                description:
                  "public function named `#{expected_name}`#{needed_msg} not found in semantic analysis of module.",
                file: module.file,
                line: module.line
            end
          end)
      end

    Enum.each(nifs, &validate_nif!(&1))

    %{module | nifs: nifs}
  end

  defp make_nif(sema_function, module) do
    sema_function.name
    |> Nif.new(module, [])
    |> Nif.set_file_line(module.manifest_module, module.parsed)
    |> struct!(
      signature: sema_function,
      params: params_from_sema(sema_function, module),
      return: Return.new(sema_function.return, cleanup: true)
    )
  end

  defp params_from_sema(sema_function, module) do
    sema_function.params
    |> Enum.with_index(fn
      param_type, index ->
        {index, Parameter.new(param_type, [], module)}
    end)
    |> Map.new()
  end

  defp maybe_merge(nif, specified_nifs, _module) do
    if to_merge = Enum.find(specified_nifs, &functions_match?(&1, nif)) do
      merge_nif(nif, to_merge)
    else
      nif
    end
  end

  defp functions_match?(a, b) do
    dbg()
    raise "unimplemented"
  end

  defp merge_nif(_, _) do
    raise "unimplemented"
  end

  # converts a semantic analysis function struct into a nif struct.

  # defp to_nif(
  #       nif,
  #       %Function{
  #         arity: 3,
  #         params: [:env, %Integer{}, %Manypointer{child: t}],
  #         return: t
  #       } = sema,
  #       opts
  #     )
  #     when t in ~w[term erl_nif_term]a do
  #  arities =
  #    case Keyword.fetch(opts, :arity) do
  #      {:ok, arity} when arity in 0..63 ->
  #        arities(arity)
  #
  #      {:ok, %Range{} = range} ->
  #        arities(range)
  #
  #      {:ok, list} when is_list(list) ->
  #        Enum.flat_map(list, &arities/1)
  #
  #      :error ->
  #        raise CompileError,
  #          description:
  #            "the raw function #{inspect(nif.module)}.#{nif.name}/? must have arities specified in zigler parameters",
  #          file: nif.file,
  #          line: nif.line
  #    end
  #
  #  %{nif | signature: sema, raw: t, params: arities, return: Return.new(:raw, t)}
  # end
  #
  #  defp to_nif(%{params: nif_params!} = nif, sema, opts) do
  #    nif_params! |> dbg(limit: 25)
  #
  #    nif_params! = nif_params! || %{}
  #
  #    Enum.each(nif_params!, fn
  #      {index, _} ->
  #        if index >= sema.arity do
  #          raise CompileError,
  #            description:
  #              "nif function `#{nif.name}` has an arity of #{sema.arity}, but parameter #{index} was specified",
  #            file: nif.file,
  #            line: nif.line
  #        end
  #    end)
  #
  #    %{
  #      nif
  #      | signature: sema,
  #        params: params_to_nif(sema, nif_params!),
  #        return: return_to_nif(sema, opts ++ [cleanup: nif.cleanup])
  #    }
  #  end

  defp arities(integer) when is_integer(integer), do: [integer]
  defp arities(start..finish//1), do: Enum.to_list(start..finish)

  defp params_to_nif(%{params: params}, opts) do
  end

  defp validate_nif!(%{raw: nil} = nif) do
    Enum.each(nif.params, &validate_param!(&1, nif))
    validate_return!(nif)
  end

  defp validate_nif!(_raw_nif), do: :ok

  defp validate_param!({_, %{in_out: true} = param}, nif) do
    unless Type.in_out_allowed?(param.type) do
      raise CompileError,
        description:
          "nif function `#{nif.name}` cannot have a an in-out parameter of type #{Type.render_zig(param.type)}",
        file: nif.file,
        line: nif.line
    end
  end

  defp validate_param!({_, param}, nif) do
    unless Type.get_allowed?(param.type) do
      raise CompileError,
        description:
          "nif function `#{nif.name}` cannot have a value of type #{Type.render_zig(param.type)} as a parameter",
        file: nif.file,
        line: nif.line
    end
  end

  defp validate_return!(nif) do
    unless Type.make_allowed?(nif.return.type) do
      raise CompileError,
        description:
          "nif function `#{nif.name}` cannot return a value of type #{Type.render_zig(nif.return.type)}",
        file: nif.file,
        line: nif.line
    end
  end

  defp check_invalid_callbacks!(module) do
    Enum.each(module.sema.callbacks, fn
      {type, nil} ->
        seek_and_raise!(
          type,
          module,
          &"#{type} callback #{&1}must be declared `pub`",
          &"#{type} callback #{&1}not found"
        )

      {:on_load, %{arity: arity}} when arity not in [2, 3] ->
        seek_and_raise!(:on_load, module, &"on_load callback #{&1}must have arity 2 or 3")

      {:on_load, %{arity: 2} = function} ->
        case function.params do
          [%Pointer{optional: true, child: %Pointer{optional: true}}, second] ->
            if Type.get_allowed?(second) do
              :ok
            else
              seek_and_raise!(
                :on_load,
                module,
                &"on_load (automatic-style) callback #{&1}must have a second parameter of a type compatible with `beam.get`.\n\n    got: `#{Type.render_zig(second)}`"
              )
            end

          [first, _] ->
            seek_and_raise!(
              :on_load,
              module,
              &"on_load (automatic-style) callback #{&1}must have a first paramater of a `?*?*` type.\n\n    got: `#{Type.render_zig(first)}`"
            )
        end

        case function.return do
          :void ->
            :ok

          %Integer{} ->
            :ok

          %Zig.Type.Enum{} ->
            :ok

          %Error{child: :void} ->
            :ok

          bad ->
            seek_and_raise!(
              :on_load,
              module,
              &"on_load (automatic-style) callback #{&1}must have a return of type integer, enum, `void`, or `!void`.\n\n    got: `#{Type.render_zig(bad)}`"
            )
        end

      {:on_load, %{arity: 3} = function} ->
        case function.params do
          [:env, %Pointer{optional: true, child: %Pointer{optional: true}}, :erl_nif_term] ->
            :ok

          [first, _, _] when first != :env ->
            seek_and_raise!(
              :on_load,
              module,
              &"on_load (raw-style) callback #{&1}must have a first parameter of type `beam.env`.\n\n    got: `#{Type.render_zig(first)}`"
            )

          [_, _, third] when third != :erl_nif_term ->
            seek_and_raise!(
              :on_load,
              module,
              &"on_load (raw-style) callback #{&1}must have a third parameter of type `e.ErlNifTerm`.\n\n    got: `#{Type.render_zig(third)}`"
            )

          [_, second, _] ->
            seek_and_raise!(
              :on_load,
              module,
              &"on_load (raw-style) callback #{&1}must have a second parameter of type `?*?*`.\n\n    got: `#{Type.render_zig(second)}`"
            )
        end

        case function.return do
          %Integer{signedness: :signed, bits: 32} ->
            :ok

          bad ->
            seek_and_raise!(
              :on_load,
              module,
              &"on_load (raw-style) callback #{&1}must have return type `c_int`.\n\n    got: `#{Type.render_zig(bad)}`"
            )
        end

      {:on_upgrade, %{arity: arity}} when arity not in [3, 4] ->
        seek_and_raise!(:on_upgrade, module, &"on_upgrade callback #{&1}must have arity 3 or 4")

      {:on_upgrade, %{arity: 3} = function} ->
        case function.params do
          [
            %Pointer{optional: true, child: %Pointer{optional: true}},
            %Pointer{optional: true, child: %Pointer{optional: true}},
            third
          ] ->
            if Type.get_allowed?(third) do
              :ok
            else
              seek_and_raise!(
                :on_upgrade,
                module,
                &"on_upgrade (automatic-style) callback #{&1}must have a third parameter of a type compatible with `beam.get`.\n\n    got: `#{Type.render_zig(third)}`"
              )
            end

          [%Pointer{optional: true, child: %Pointer{optional: true}}, second, _] ->
            seek_and_raise!(
              :on_upgrade,
              module,
              &"on_upgrade (automatic-style) callback #{&1}must have a second parameter of type `?*?*`.\n\n    got: `#{Type.render_zig(second)}`"
            )

          [first, _, _] ->
            seek_and_raise!(
              :on_upgrade,
              module,
              &"on_upgrade (automatic-style) callback #{&1}must have a first parameter of type `?*?*`.\n\n    got: `#{Type.render_zig(first)}`"
            )
        end

        case function.return do
          :void ->
            :ok

          %Integer{} ->
            :ok

          %Zig.Type.Enum{} ->
            :ok

          %Error{child: :void} ->
            :ok

          bad ->
            seek_and_raise!(
              :on_upgrade,
              module,
              &"on_upgrade (automatic-style) callback #{&1}must have an integer, enum, `void`, or `!void` as a return.\n\n    got: `#{Type.render_zig(bad)}`"
            )
        end

      {:on_upgrade, %{arity: 4} = function} ->
        case function.params do
          [
            :env,
            %Pointer{optional: true, child: %Pointer{optional: true}},
            %Pointer{optional: true, child: %Pointer{optional: true}},
            :erl_nif_term
          ] ->
            :ok

          [
            :env,
            %Pointer{optional: true, child: %Pointer{optional: true}},
            %Pointer{optional: true, child: %Pointer{optional: true}},
            fourth
          ] ->
            seek_and_raise!(
              :on_upgrade,
              module,
              &"on_upgrade (raw-style) callback #{&1}must have a fourth parameter of type `e.ErlNifTerm`.\n\n    got: `#{Type.render_zig(fourth)}`"
            )

          [
            :env,
            %Pointer{optional: true, child: %Pointer{optional: true}},
            third,
            _
          ] ->
            seek_and_raise!(
              :on_upgrade,
              module,
              &"on_upgrade (raw-style) callback #{&1}must have a third parameter of type `?*?*`.\n\n    got: `#{Type.render_zig(third)}`"
            )

          [
            :env,
            second,
            _,
            _
          ] ->
            seek_and_raise!(
              :on_upgrade,
              module,
              &"on_upgrade (raw-style) callback #{&1}must have a second parameter of type `?*?*`.\n\n    got: `#{Type.render_zig(second)}`"
            )

          [first, _, _, _] ->
            seek_and_raise!(
              :on_upgrade,
              module,
              &"on_upgrade (raw-style) callback #{&1}must have a first parameter of type `beam.env`.\n\n    got: `#{Type.render_zig(first)}`"
            )
        end

        case function.return do
          %Integer{signedness: :signed, bits: 32} ->
            :ok

          bad ->
            seek_and_raise!(
              :on_upgrade,
              module,
              &"on_upgrade (raw-style) callback #{&1}must have an `c_int` as a return.\n\n    got: `#{Type.render_zig(bad)}`"
            )
        end

      {:on_unload, %{arity: arity}} when arity not in [1, 2] ->
        seek_and_raise!(:on_unload, module, &"on_unload callback #{&1}must have arity 1 or 2")

      {:on_unload, %{arity: 1} = function} ->
        case function.params do
          [%Pointer{optional: true}] ->
            :ok

          [first] ->
            seek_and_raise!(
              :on_unload,
              module,
              &"on_unload (automatic-style) callback #{&1}must have a parameter of type `?*`.\n\n    got: `#{Type.render_zig(first)}`"
            )
        end

        case function.return do
          :void ->
            :ok

          bad ->
            seek_and_raise!(
              :on_unload,
              module,
              &"on_unload (automatic-style) callback #{&1}must have `void` as a return.\n\n    got: `#{Type.render_zig(bad)}`"
            )
        end

      {:on_unload, %{arity: 2} = function} ->
        case function.params do
          [:env, %Pointer{optional: true}] ->
            :ok

          [first, %Pointer{optional: true}] ->
            seek_and_raise!(
              :on_unload,
              module,
              &"on_unload (raw-style) callback #{&1}must have a first parameter of type `beam.env`.\n\n    got: `#{Type.render_zig(first)}`"
            )

          [_, second] ->
            seek_and_raise!(
              :on_unload,
              module,
              &"on_unload (raw-style) callback #{&1}must have a second parameter of type `?*`.\n\n    got: `#{Type.render_zig(second)}`"
            )
        end

        case function.return do
          :void ->
            :ok

          bad ->
            seek_and_raise!(
              :on_unload,
              module,
              &"on_unload (raw-style) callback #{&1}must have `void` as a return.\n\n    got: `#{Type.render_zig(bad)}`"
            )
        end

      _ ->
        :ok
    end)
  end

  defp seek_and_raise!(type, module, error1, error2 \\ nil) do
    name = module.callbacks[type]
    print_name = unless type == name, do: "#{name} "

    for %{name: ^name, location: {line, _col}} <- module.parsed.code do
      {file, line} =
        module.manifest_module.__resolve(%{file_name: module.zig_code_path, line: line})

      raise CompileError,
        description: error1.(print_name),
        file: file,
        line: line
    end

    raise CompileError,
      description: if(error2, do: error2.(print_name), else: error1.(print_name)),
      file: module.file,
      line: module.line
  end
end
