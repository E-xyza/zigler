defmodule Zig.Sema do
  @moduledoc false
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

  @type info :: %__MODULE__{
          functions: [Function.t()],
          types: keyword(Type.t()),
          decls: keyword(Type.t()),
          callbacks: [Function.t()]
        }

  # PHASE 1:  SEMA EXECUTION

  # Documentation helper function for zig_doc compatibility
  # This is a simpler version that works with just a file path for doc generation
  def run_sema_doc(file_path) do
    file_path
    |> Zig.Command.run_sema_doc!()
    |> Zig._json_decode!()
    |> integrate_sema(%{module: nil})
  end

  @spec run_sema!(Module.t()) :: Module.t()
  # performs the first stage of semantic analysis:
  # actually executing the zig command to obtaitest/mark_as_impl_test.exsn the semantic analysis of the
  # desired file.
  def run_sema!(module) do
    {module, json} = _obtain_precompiled_sema_json(module)

    json_map =
      if module.precompiled do
        Zig._json_decode!(json)
      else
        module
        |> Zig.Command.run_sema!()
        |> Zig._json_decode!()
        |> maybe_dump(module)
        |> reject_ignored(module)
        |> reject_allocators(module)
        |> reject_error_interpreters(module)
      end

    sema =
      json_map
      |> assign_callbacks(module)
      |> integrate_sema(module)

    sema_json =
      json_map
      |> Zig._json_encode!()
      |> IO.iodata_to_binary()

    %{module | sema: sema, sema_json: sema_json}
  rescue
    e in Zig.CompileError ->
      resolved = Zig.CompileError.resolve(e, module)
      Logger.error("sema error: #{Exception.message(resolved)}")
      reraise resolved, __STACKTRACE__
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
    # {f
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
         } = sema_json,
         module
       ) do
    callbacks = List.wrap(sema_json["callbacks"])

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
      sema_json_pretty = Zig._json_encode!(sema_json, pretty: true)
      IO.puts([IO.ANSI.yellow(), sema_json_pretty, IO.ANSI.reset()])
    end

    sema_json
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
          |> Enum.reduce(specified_nifs, &add_missing_nif(&1, &2, module))
          |> build_from_specs(sema_functions, module)

        specified_nifs when is_list(specified_nifs) ->
          build_from_specs(specified_nifs, sema_functions, module)
      end

    Enum.each(nifs, &validate_nif!(&1))

    %{module | nifs: nifs}
  end

  defp add_missing_nif(sema_function, so_far, module) do
    if Enum.any?(so_far, &(&1.name == sema_function.name)) do
      so_far
    else
      [make_default_nif(sema_function, module) | so_far]
    end
  end

  defp build_from_specs(specified_nifs, sema_functions, module) do
    Enum.map(specified_nifs, &build_nif_from_spec(&1, sema_functions, module))
  end

  defp build_nif_from_spec(nif, sema_functions, module) do
    expected_name = nif.alias || nif.name

    case Enum.find(sema_functions, &(&1.name == expected_name)) do
      nil -> raise_missing_function_error(expected_name, nif, module)
      sema_function -> make_default_nif(sema_function, module) |> Nif.merge(nif)
    end
  end

  defp raise_missing_function_error(expected_name, nif, module) do
    needed_msg = if nif.alias, do: " (needed by nif #{nif.name})", else: ""

    raise CompileError,
      description:
        "public function named `#{expected_name}`#{needed_msg} not found in semantic analysis of module.",
      file: module.file,
      line: module.line
  end

  @module_settings ~w[module file line module_code_path zig_code_path]a

  defp make_default_nif(sema_function, module) do
    context = Options.initialize_context(module, module.otp_app)
    cleanup = Keyword.get(module.default_nif_opts, :cleanup, true)

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
    Enum.each(module.sema.callbacks, &check_invalid_callback!(&1, module))
  end

  defp check_invalid_callback!({type, nil}, module) do
    seek_and_raise!(
      type,
      module,
      &"#{type} callback #{&1}must be declared `pub`",
      &"#{type} callback #{&1}not found"
    )
  end

  defp check_invalid_callback!({:on_load, %{arity: arity}}, module) when arity not in [2, 3] do
    seek_and_raise!(:on_load, module, &"on_load callback #{&1}must have arity 2 or 3")
  end

  defp check_invalid_callback!({:on_load, %{arity: 2} = function}, module) do
    check_on_load_auto_params!(function.params, module)
    check_on_load_auto_return!(function.return, module)
  end

  defp check_invalid_callback!({:on_load, %{arity: 3} = function}, module) do
    check_on_load_raw_params!(function.params, module)
    check_on_load_raw_return!(function.return, module)
  end

  defp check_invalid_callback!({:on_upgrade, %{arity: arity}}, module) when arity not in [3, 4] do
    seek_and_raise!(:on_upgrade, module, &"on_upgrade callback #{&1}must have arity 3 or 4")
  end

  defp check_invalid_callback!({:on_upgrade, %{arity: 3} = function}, module) do
    check_on_upgrade_auto_params!(function.params, module)
    check_on_upgrade_auto_return!(function.return, module)
  end

  defp check_invalid_callback!({:on_upgrade, %{arity: 4} = function}, module) do
    check_on_upgrade_raw_params!(function.params, module)
    check_on_upgrade_raw_return!(function.return, module)
  end

  defp check_invalid_callback!({:on_unload, %{arity: arity}}, module) when arity not in [1, 2] do
    seek_and_raise!(:on_unload, module, &"on_unload callback #{&1}must have arity 1 or 2")
  end

  defp check_invalid_callback!({:on_unload, %{arity: 1} = function}, module) do
    check_on_unload_auto_params!(function.params, module)
    check_on_unload_auto_return!(function.return, module)
  end

  defp check_invalid_callback!({:on_unload, %{arity: 2} = function}, module) do
    check_on_unload_raw_params!(function.params, module)
    check_on_unload_raw_return!(function.return, module)
  end

  defp check_invalid_callback!(_, _module), do: :ok

  # on_load automatic style (arity 2)
  defp check_on_load_auto_params!([%Pointer{optional: true, child: %Pointer{optional: true}}, second], module) do
    unless Type.get_allowed?(second) do
      seek_and_raise!(
        :on_load,
        module,
        &"on_load (automatic-style) callback #{&1}must have a second parameter of a type compatible with `beam.get`.\n\n    got: `#{Type.render_zig(second)}`"
      )
    end
  end

  defp check_on_load_auto_params!([first, _], module) do
    seek_and_raise!(
      :on_load,
      module,
      &"on_load (automatic-style) callback #{&1}must have a first paramater of a `?*?*` type.\n\n    got: `#{Type.render_zig(first)}`"
    )
  end

  defp check_on_load_auto_return!(ret, _module) when ret in [:void] or is_struct(ret, Integer) or is_struct(ret, Zig.Type.Enum), do: :ok
  defp check_on_load_auto_return!(%Error{child: :void}, _module), do: :ok

  defp check_on_load_auto_return!(bad, module) do
    seek_and_raise!(
      :on_load,
      module,
      &"on_load (automatic-style) callback #{&1}must have a return of type integer, enum, `void`, or `!void`.\n\n    got: `#{Type.render_zig(bad)}`"
    )
  end

  # on_load raw style (arity 3)
  defp check_on_load_raw_params!([:env, %Pointer{optional: true, child: %Pointer{optional: true}}, :erl_nif_term], _module), do: :ok

  defp check_on_load_raw_params!([first, _, _], module) when first != :env do
    seek_and_raise!(
      :on_load,
      module,
      &"on_load (raw-style) callback #{&1}must have a first parameter of type `beam.env`.\n\n    got: `#{Type.render_zig(first)}`"
    )
  end

  defp check_on_load_raw_params!([_, _, third], module) when third != :erl_nif_term do
    seek_and_raise!(
      :on_load,
      module,
      &"on_load (raw-style) callback #{&1}must have a third parameter of type `e.ErlNifTerm`.\n\n    got: `#{Type.render_zig(third)}`"
    )
  end

  defp check_on_load_raw_params!([_, second, _], module) do
    seek_and_raise!(
      :on_load,
      module,
      &"on_load (raw-style) callback #{&1}must have a second parameter of type `?*?*`.\n\n    got: `#{Type.render_zig(second)}`"
    )
  end

  defp check_on_load_raw_return!(%Integer{signedness: :signed, bits: 32}, _module), do: :ok

  defp check_on_load_raw_return!(bad, module) do
    seek_and_raise!(
      :on_load,
      module,
      &"on_load (raw-style) callback #{&1}must have return type `c_int`.\n\n    got: `#{Type.render_zig(bad)}`"
    )
  end

  # on_upgrade automatic style (arity 3)
  defp check_on_upgrade_auto_params!([%Pointer{optional: true, child: %Pointer{optional: true}}, %Pointer{optional: true, child: %Pointer{optional: true}}, third], module) do
    unless Type.get_allowed?(third) do
      seek_and_raise!(
        :on_upgrade,
        module,
        &"on_upgrade (automatic-style) callback #{&1}must have a third parameter of a type compatible with `beam.get`.\n\n    got: `#{Type.render_zig(third)}`"
      )
    end
  end

  defp check_on_upgrade_auto_params!([%Pointer{optional: true, child: %Pointer{optional: true}}, second, _], module) do
    seek_and_raise!(
      :on_upgrade,
      module,
      &"on_upgrade (automatic-style) callback #{&1}must have a second parameter of type `?*?*`.\n\n    got: `#{Type.render_zig(second)}`"
    )
  end

  defp check_on_upgrade_auto_params!([first, _, _], module) do
    seek_and_raise!(
      :on_upgrade,
      module,
      &"on_upgrade (automatic-style) callback #{&1}must have a first parameter of type `?*?*`.\n\n    got: `#{Type.render_zig(first)}`"
    )
  end

  defp check_on_upgrade_auto_return!(ret, _module) when ret in [:void] or is_struct(ret, Integer) or is_struct(ret, Zig.Type.Enum), do: :ok
  defp check_on_upgrade_auto_return!(%Error{child: :void}, _module), do: :ok

  defp check_on_upgrade_auto_return!(bad, module) do
    seek_and_raise!(
      :on_upgrade,
      module,
      &"on_upgrade (automatic-style) callback #{&1}must have an integer, enum, `void`, or `!void` as a return.\n\n    got: `#{Type.render_zig(bad)}`"
    )
  end

  # on_upgrade raw style (arity 4)
  defp check_on_upgrade_raw_params!([:env, %Pointer{optional: true, child: %Pointer{optional: true}}, %Pointer{optional: true, child: %Pointer{optional: true}}, :erl_nif_term], _module), do: :ok

  defp check_on_upgrade_raw_params!([:env, %Pointer{optional: true, child: %Pointer{optional: true}}, %Pointer{optional: true, child: %Pointer{optional: true}}, fourth], module) do
    seek_and_raise!(
      :on_upgrade,
      module,
      &"on_upgrade (raw-style) callback #{&1}must have a fourth parameter of type `e.ErlNifTerm`.\n\n    got: `#{Type.render_zig(fourth)}`"
    )
  end

  defp check_on_upgrade_raw_params!([:env, %Pointer{optional: true, child: %Pointer{optional: true}}, third, _], module) do
    seek_and_raise!(
      :on_upgrade,
      module,
      &"on_upgrade (raw-style) callback #{&1}must have a third parameter of type `?*?*`.\n\n    got: `#{Type.render_zig(third)}`"
    )
  end

  defp check_on_upgrade_raw_params!([:env, second, _, _], module) do
    seek_and_raise!(
      :on_upgrade,
      module,
      &"on_upgrade (raw-style) callback #{&1}must have a second parameter of type `?*?*`.\n\n    got: `#{Type.render_zig(second)}`"
    )
  end

  defp check_on_upgrade_raw_params!([first, _, _, _], module) do
    seek_and_raise!(
      :on_upgrade,
      module,
      &"on_upgrade (raw-style) callback #{&1}must have a first parameter of type `beam.env`.\n\n    got: `#{Type.render_zig(first)}`"
    )
  end

  defp check_on_upgrade_raw_return!(%Integer{signedness: :signed, bits: 32}, _module), do: :ok

  defp check_on_upgrade_raw_return!(bad, module) do
    seek_and_raise!(
      :on_upgrade,
      module,
      &"on_upgrade (raw-style) callback #{&1}must have an `c_int` as a return.\n\n    got: `#{Type.render_zig(bad)}`"
    )
  end

  # on_unload automatic style (arity 1)
  defp check_on_unload_auto_params!([%Pointer{optional: true}], _module), do: :ok

  defp check_on_unload_auto_params!([first], module) do
    seek_and_raise!(
      :on_unload,
      module,
      &"on_unload (automatic-style) callback #{&1}must have a parameter of type `?*`.\n\n    got: `#{Type.render_zig(first)}`"
    )
  end

  defp check_on_unload_auto_return!(:void, _module), do: :ok

  defp check_on_unload_auto_return!(bad, module) do
    seek_and_raise!(
      :on_unload,
      module,
      &"on_unload (automatic-style) callback #{&1}must have `void` as a return.\n\n    got: `#{Type.render_zig(bad)}`"
    )
  end

  # on_unload raw style (arity 2)
  defp check_on_unload_raw_params!([:env, %Pointer{optional: true}], _module), do: :ok

  defp check_on_unload_raw_params!([first, %Pointer{optional: true}], module) do
    seek_and_raise!(
      :on_unload,
      module,
      &"on_unload (raw-style) callback #{&1}must have a first parameter of type `beam.env`.\n\n    got: `#{Type.render_zig(first)}`"
    )
  end

  defp check_on_unload_raw_params!([_, second], module) do
    seek_and_raise!(
      :on_unload,
      module,
      &"on_unload (raw-style) callback #{&1}must have a second parameter of type `?*`.\n\n    got: `#{Type.render_zig(second)}`"
    )
  end

  defp check_on_unload_raw_return!(:void, _module), do: :ok

  defp check_on_unload_raw_return!(bad, module) do
    seek_and_raise!(
      :on_unload,
      module,
      &"on_unload (raw-style) callback #{&1}must have `void` as a return.\n\n    got: `#{Type.render_zig(bad)}`"
    )
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

  def _obtain_precompiled_sema_json(%{precompiled: nil} = module), do: {module, nil}

  def _obtain_precompiled_sema_json(%{precompiled: {:web, address, shasum}} = module) do
    file = http_get!(address)

    {dll_shasum, pdb_shasum} = normalize_shasum_parts(shasum)

    found_hash =
      :sha256
      |> :crypto.hash(file)
      |> Base.encode16(case: :lower)

    found_hash == String.downcase(dll_shasum) ||
      raise "hash mismatch: expected #{dll_shasum}, got #{found_hash}"

    staging_dir = Zig.Builder.staging_directory(module.module)
    staging_path = Path.join(staging_dir, Path.basename(address))

    File.mkdir_p!(staging_dir)
    File.write!(staging_path, file)

    # Download PDB file for Windows targets (optional - may not exist for all releases)
    maybe_download_pdb(address, staging_path, pdb_shasum)

    _obtain_precompiled_sema_json(%{module | precompiled: staging_path})
  end

  defp maybe_download_pdb(address, staging_path, pdb_shasum) do
    if String.ends_with?(address, ".dll") do
      pdb_address = String.replace_suffix(address, ".dll", ".pdb")
      download_pdb(pdb_address, staging_path, pdb_shasum)
    end
  end

  defp download_pdb(pdb_address, staging_path, pdb_shasum) do
    case http_get(pdb_address) do
      {:ok, pdb_file} ->
        verify_pdb_hash(pdb_file, pdb_shasum)
        pdb_staging_path = String.replace_suffix(staging_path, ".dll", ".pdb")
        File.write!(pdb_staging_path, pdb_file)

      {:error, _} ->
        Logger.debug("PDB file not available at #{pdb_address}")
    end
  end

  defp verify_pdb_hash(_pdb_file, nil), do: :ok

  defp verify_pdb_hash(pdb_file, pdb_shasum) do
    found_pdb_hash =
      :sha256
      |> :crypto.hash(pdb_file)
      |> Base.encode16(case: :lower)

    found_pdb_hash == String.downcase(pdb_shasum) ||
      raise "PDB hash mismatch: expected #{pdb_shasum}, got #{found_pdb_hash}"
  end

  case :os.type() do
    {:unix, :darwin} ->
      def _obtain_precompiled_sema_json(%{precompiled: file} = module) do
        case System.cmd("otool", ["-s", "__DATA", "__sema", file]) do
          {out, 0} -> {module, parse_otool_sema(out)}
          {_, other} -> raise "error obtaining semantic analysis from #{file} (#{other})"
        end
      end

      defp parse_otool_sema(otool_output) do
        otool_output
        |> String.split("\n")
        |> Enum.drop(2)
        |> Enum.map(&parse_otool_line/1)
        |> IO.iodata_to_binary()
        |> Base.decode16!(case: :mixed)
        |> String.trim(<<0>>)
      end

      defp parse_otool_line(line) do
        line
        |> String.trim()
        |> String.split()
        |> Enum.drop(1)
        |> Enum.map(&endian_reverse/1)
      end

      defp endian_reverse(str) do
        str
        |> String.to_charlist()
        |> Enum.chunk_every(2)
        |> Enum.reverse()
      end

    {:unix, bsd} when bsd in [:freebsd, :openbsd] ->
      # freebsd/openbsd - BSD objcopy doesn't support --dump-section=/dev/stdout
      def _obtain_precompiled_sema_json(%{precompiled: file} = module) do
        tmp_path =
          16
          |> :crypto.strong_rand_bytes()
          |> Base.encode16(case: :lower)
          |> then(&Path.join(Zig._tmp_dir(), &1))

        File.touch!(tmp_path)

        case System.cmd("objcopy", ["-O", "binary", "-j", ".sema", file, tmp_path]) do
          {_, 0} ->
            {module, tmp_path |> File.read!() |> String.trim_trailing(<<0>>)}

          {_, other} ->
            raise "error obtaining semantic analysis from #{file} (#{other})"
        end
      end

    {:unix, _} ->
      # linux
      def _obtain_precompiled_sema_json(%{precompiled: file} = module) do
        case System.cmd("objcopy", ["--dump-section", ".sema=/dev/stdout", file]) do
          {json, 0} -> {module, String.trim_trailing(json, <<0>>)}
          {_, other} -> raise "error obtaining semantic analysis from #{file} (#{other})"
        end
      end

    {_, :nt} ->
      def _obtain_precompiled_sema_json(%{precompiled: file} = module) do
        tmp_path =
          16
          |> :crypto.strong_rand_bytes()
          |> Base.encode16(case: :lower)
          |> then(&Path.join(Zig._tmp_dir(), &1))

        case System.cmd("objcopy", ["--dump-section", ".sema=#{tmp_path}", file]) do
          {_, 0} ->
            System.cmd("objdump", ["-s", "-j", ".sema", file])
            {module, tmp_path |> File.read!() |> String.trim_trailing(<<0>>)}

          {_, other} ->
            raise "error obtaining semantic analysis from #{file} (#{other})"
        end
      end
  end

  # Extract DLL and PDB shasums from either the new map format or legacy string format
  defp normalize_shasum_parts(%{dll: dll_sha, pdb: pdb_sha}), do: {dll_sha, pdb_sha}
  defp normalize_shasum_parts(shasum) when is_binary(shasum), do: {shasum, nil}

  @otp_version :otp_release
               |> :erlang.system_info()
               |> List.to_integer()

  if @otp_version >= 25 do
    defp ssl_opts do
      [
        verify: :verify_peer,
        cacerts: :public_key.cacerts_get()
      ]
    end
  else
    defp ssl_opts do
      # unfortunately in otp 24 there is not a clean way of obtaining cacerts
      []
    end
  end

  defp http_get!(url) do
    case http_get(url) do
      {:ok, body} -> body
      {:error, reason} -> raise "HTTP GET failed for #{url}: #{inspect(reason)}"
    end
  end

  defp http_get(url) do
    case :httpc.request(
           :get,
           {url, []},
           [
             ssl:
               [
                 depth: 100,
                 customize_hostname_check: [
                   match_fun: :public_key.pkix_verify_hostname_match_fun(:https)
                 ]
               ] ++ ssl_opts()
           ],
           body_format: :binary
         ) do
      {:ok, {{_, 200, _}, _headers, body}} -> {:ok, body}
      {:ok, {{_, status, _}, _headers, _body}} -> {:error, {:http_status, status}}
      {:error, reason} -> {:error, reason}
    end
  end
end
