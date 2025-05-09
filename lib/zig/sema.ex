defmodule Zig.Sema do
  @moduledoc false
  require EEx
  alias Zig.Attributes
  alias Zig.Module
  alias Zig.Nif
  alias Zig.Options
  alias Zig.Parameter
  alias Zig.Return
  alias Zig.Type
  alias Zig.Type.Error
  alias Zig.Type.Function
  alias Zig.Type.Integer
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

    allocators = Enum.flat_map(nifs, &List.wrap(if allocator = &1.allocator, do: "#{allocator}"))

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
      |> Enum.flat_map(&error_fun/1)
      |> Enum.reject(&Keyword.has_key?(nifs, &1))
      |> Enum.map(&"#{&1}")

    Map.update!(json, "functions", fn
      functions ->
        Enum.reject(functions, &(&1["name"] in error_functions))
    end)
  end

  defp error_fun(nif), do: List.wrap(nif.return.error)

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
  def analyze_file!(%{sema: %{functions: sema_functions}} = module) do
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
          # go through the list of sema_functions and add them to the specified nifs list
          # if they aren't there yet.

          sema_functions
          |> Enum.reduce(specified_nifs, fn
            sema_function, so_far ->
              if Enum.any?(so_far, &(&1.name == sema_function.name)) do
                so_far
              else
                [make_default_nif(sema_function, module) | so_far]
              end
          end)
          |> build_from_specs(sema_functions, module)

        specified_nifs when is_list(specified_nifs) ->
          build_from_specs(specified_nifs, sema_functions, module)
      end

    Enum.each(nifs, &validate_nif!(&1))

    %{module | nifs: nifs}
  end

  defp build_from_specs(specified_nifs, sema_functions, module) do
    Enum.map(specified_nifs, fn nif ->
      expected_name = nif.alias || nif.name

      if sema_function = Enum.find(sema_functions, &(&1.name == expected_name)) do
        sema_function
        |> make_default_nif(module)
        |> Nif.merge(nif)
      else
        needed_msg = if nif.alias, do: " (needed by nif #{nif.name})"

        raise CompileError,
          description:
            "public function named `#{expected_name}`#{needed_msg} not found in semantic analysis of module.",
          file: module.file,
          line: module.line
      end
    end)
  end

  @module_settings ~w[module file line module_code_path zig_code_path]a

  defp make_default_nif(sema_function, module) do
    context = Options.initialize_context(module, module.otp_app)
    cleanup =  Keyword.get(module.default_nif_opts, :cleanup, true)

    @module_settings
    |> Enum.map(&{&1, Map.fetch!(module, &1)})
    |> Keyword.merge(module.default_nif_opts)
    |> then(&{sema_function.name, &1})
    |> Nif.new(context)
    |> Nif.set_file_line(module.manifest_module, module.parsed)
    |> struct!(
      arity: nil,
      signature: sema_function,
      raw: Function.raw(sema_function),
      params: params_from_sema(sema_function, cleanup),
      return: %Return{type: sema_function.return}
    )
  end

  defp params_from_sema(sema_function, cleanup) do
    sema_function.params
    |> Enum.with_index(&{&2, %Parameter{type: &1, cleanup: cleanup}})
    |> Map.new()
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
