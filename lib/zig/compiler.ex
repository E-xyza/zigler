defmodule Zig.Compiler do
  @moduledoc """
  handles instrumenting elixir code with hooks for zig NIFs.
  """

  @enforce_keys [:assembly_dir, :code_file, :assembly, :module_spec]

  # contains critical information for the compilation.
  defstruct @enforce_keys ++ [compiler_target: nil, test_dirs: []]

  alias Zig.Assembler

  @type t :: %__MODULE__{
    assembly_dir:    Path.t,
    assembly:        [Assembler.t],
    code_file:       Path.t,
    module_spec:     Zig.Module.t,
    compiler_target: atom
  }

  require Logger

  alias Zig.Command

  @zig_dir_path Path.expand("../../../zig", __ENV__.file)

  defmacro __before_compile__(context) do

    ###########################################################################
    # VERIFICATION

    module = Module.get_attribute(context.module, :zigler)
    Module.register_attribute(context.module, :nif_code_map, persist: true)

    zig_tree = Path.join(@zig_dir_path, Command.version_name(module.zig_version))

    # check to see if the zig version has been downloaded.  If not,
    # go ahead and download it.
    unless module.local_zig or File.dir?(zig_tree) do
      Command.fetch("#{module.zig_version}")
    end

    if module.nifs == [] do
      raise CompileError,
        file: context.file,
        description: "no nifs found in the module #{context.module}"
    end

    ###########################################################################
    # COMPILATION STEPS

    compiled = compilation(module, zig_tree)

    ###########################################################################
    # MACRO STEPS

    dependencies = dependencies_for(compiled.assembly)
    nif_functions = Enum.map(module.nifs, &function_skeleton/1)
    nif_name = Zig.nif_name(module, false)

    # TODO: merge these two.
    if module.dry_run do
      quote do
        unquote_splicing(dependencies)
        unquote_splicing(nif_functions)
        unquote(exception_for(module))
        def __load_nifs__, do: :ok
      end
    else
      quote do
        import Logger
        unquote_splicing(dependencies)
        unquote_splicing(nif_functions)
        unquote(exception_for(module))
        def __load_nifs__ do
          # LOADS the nifs from :code.lib_dir() <> "ebin", which is
          # a path that has files correctly moved in to release packages.

          unquote(module.otp_app)
          |> :code.lib_dir()
          |> Path.join("ebin")
          |> Path.join(unquote(nif_name))
          |> String.to_charlist()
          |> :erlang.load_nif(0)
          |> case do
            :ok ->
              Logger.debug("loaded module at #{unquote(nif_name)}")
            error = {:error, any} ->
              Logger.error("loading module #{unquote(nif_name)} #{inspect any}")
          end
        end
      end
    end
  end

  defp compilation(module, zig_tree) do
    compiler = precompile(module)
    unless module.dry_run do
      Command.compile(compiler, zig_tree)
    end
    cleanup(compiler)
    compiler
  end

  defp dependencies_for(assemblies) do
    Enum.map(assemblies, fn assembly ->
      quote do
        @external_resource unquote(assembly.source)
      end
    end)
  end

  defp exception_for(mod = %{nifs: nifs}) do
    if Enum.any?(nifs, &returns_error?/1) do
      # raise a compile error, if we are running linux and we don't link_libc
      if {:unix, :linux} == :os.type() do
        unless mod.link_libc do
          raise CompileError,
            file: mod.file,
            description: "a nif module that has zig error returns compiled with debug symbols must be `link_libc: true`"
        end
      end

      quote do
        defmodule ZigError do
          defexception [:message, :error_return_trace]

          def blame(exception, stacktrace) do
            [{m, f, _, _} | _] = stacktrace
            [z] = m.__info__(:attributes)[:zigler]
            a = Enum.find(z.nifs, &(&1.name == f)).arity
            code_map =

            new_message = "#{inspect m}.#{f}/#{a} returned the zig error `.#{exception.message}`"
            zig_errors =
              Enum.map(exception.error_return_trace, fn
                {_, fun, dest_file, dest_line} ->
                  file = file_lookup(m.__info__(:attributes)[:nif_code_map], dest_file)

                  {src_file, src_line} = if file == z.file do
                    z.code
                    |> IO.iodata_to_binary
                    |> String.split("\n")
                    |> line_lookup(dest_line)
                  else
                    {file, dest_line}
                  end

                  {:zig, fun, '*', [file: src_file, line: src_line]}
              end)

            {%{exception | message: new_message}, Enum.reverse(zig_errors, stacktrace)}
          end

          defp file_lookup(code_map, dest_file) do
            :proplists.get_value(dest_file, code_map, dest_file)
          end

          defp line_lookup(code_cache, dest_line) do
            code_cache
            |> Enum.with_index(1)
            |> Enum.reduce({"", 0}, fn
              {_, ^dest_line}, fileline -> throw fileline
              {"// ref: " <> spec, _}, _ ->
                [file, "line:", line] = String.split(spec)
                {file, String.to_integer(line) + 1}
              _, {file, line} ->
                {file, line + 1}
            end)
          catch
            fileline -> fileline
          end
        end
      end
    end
  end

  defp returns_error?(%{retval: retval}) do
    match?("!" <> _, retval)
  end

  #############################################################################
  ## FUNCTION SKELETONS

  alias Zig.Nif.{DirtyCpu, DirtyIO, Synchronous, Test, Threaded, Yielding}
  alias Zig.Parser.Nif

  def function_skeleton(nif = %Nif{doc: doc}) when not is_nil(doc) do
    quote do
      @doc unquote(doc)
      unquote(function_skeleton(%{nif | doc: nil}))
    end
  end
  def function_skeleton(nif = %Nif{opts: opts, test: nil}) do
    case opts[:concurrency] do
      :threaded ->
        Threaded.beam_adapter(nif)
      :yielding ->
        Yielding.beam_adapter(nif)
      :dirty_cpu ->
        DirtyCpu.beam_adapter(nif)
      :dirty_io ->
        DirtyIO.beam_adapter(nif)
      nil ->
        Synchronous.beam_adapter(nif)
    end
  end
  def function_skeleton(nif) do
    Test.beam_adapter(nif)
  end

  #############################################################################
  ## STEPS

  def assembly_dir(env, module) do
    System.tmp_dir()
    |> String.replace("\\", "/")
    |> Path.join(".zigler_compiler/#{env}/#{module}")
  end

  @spec precompile(Zig.Module.t) :: t | no_return
  def precompile(module) do
    # build the staging directory.
    assembly_dir = assembly_dir(Mix.env, module.module)
    File.mkdir_p!(assembly_dir)

    # create the main code file
    code_file = Path.join(assembly_dir, "#{module.module}.zig")
    code_content = Zig.Code.generate_main(%{module | zig_file: code_file})

    # store it in the lookup table
    Module.put_attribute(module.module, :nif_code_map, {
      code_file,
      Path.relative_to_cwd(module.file)
    })

    # define the code file and build it.
    File.write!(code_file, code_content)

    compiler_target = Mix.target()

    # parse the module code to generate the full list of assets
    # that need to be brought in to the assembly directory
    assembly = Assembler.parse_code(code_content,
      parent_dir: Path.dirname(module.file),
      target_dir: assembly_dir,
      pub: true,
      context: [])
      ++ Enum.map(module.libs,
        &%Assembler{
          type: :library,
          source: &1,
          target: Path.basename(&1)})

    Assembler.assemble_kernel!(assembly_dir)
    Assembler.assemble_assets!(assembly, assembly_dir)

    %__MODULE__{
      assembly_dir:    assembly_dir,
      assembly:        assembly,
      code_file:       code_file,
      module_spec:     module,
      compiler_target: compiler_target
    }
  end

  @spec cleanup(t) :: :ok | no_return
  defp cleanup(compiler) do
    # in Zigler dev and test we keep our code around for debugging purposes.
    unless Mix.env() in [:dev, :test] do
      File.rm_rf!(compiler.assembly_dir)
    end
    :ok
  end
end
