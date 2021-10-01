defmodule Zig.Assembler do
  @moduledoc """
  Fist phase of the Zig compilation process.

  Looks through the contents of the zig code and creates a map
  of required source code files.  Then outputs a struct containing
  the relevant information.
  """

  require Logger

  defstruct [:type, :source, :target, context: [], pub: false]

  @type file_type :: :zig | :cinclude | :library

  @type t :: %__MODULE__{
    type: file_type,
    source: Path.t,
    target: Path.t,
    context: [String.t],
    pub: boolean
  }

  alias Zig.Parser.Imports

  defp pr(file) do
    :zigler
    |> :code.priv_dir
    |> Path.join(file)
  end

  @doc """
  assembles the "core" part of the zig adapter code, these are parts for
  shimming Zig libraries into the BEAM that are going to be @import'd into
  the runtime by default.
  """
  def assemble_kernel!(assembly_dir) do
    # copy in beam.zig
    File.cp!(pr("beam/beam.zig"), Path.join(assembly_dir, "beam.zig"))
    # copy in nif.zig
    File.cp!(pr("beam/erl_nif.zig"), Path.join(assembly_dir, "erl_nif.zig"))
    # other accessories
    File.cp!(pr("beam/beam_mutex.zig"), Path.join(assembly_dir, "beam_mutex.zig"))
    # copy in erl_nif_zig.h
    File.mkdir_p!(Path.join(assembly_dir, "include"))

    # copy "include" files in from the correct location.
    :code.root_dir()
    |> Path.join("usr/include")
    |> File.cp_r!(Path.join(assembly_dir, "include"))
  end

  @doc """
  assembles zig assets, taking them from their source to and putting them into
  the target directory.
  """
  def assemble_assets!(assembly, root_dir, for_whom) do
    Enum.each(assembly, &assemble_asset!(&1, root_dir, for_whom))
  end
  def assemble_asset!(instruction = %{type: :library}, root_dir, _for_whom) do
    target_path = Path.join(root_dir, instruction.target)
    File.cp!(instruction.source, target_path)
  end
  def assemble_asset!(instruction = %{target: {:cinclude, target}}, root_dir, _for_whom) do
    if File.exists?(instruction.source) do
      # make sure the include directory exists.
      include_dir = Path.join(root_dir, "include")
      File.mkdir_p!(include_dir)
      # send in the file.
      target_path = Path.join(include_dir, target)
      File.cp!(instruction.source, target_path)
      Logger.debug("copied #{instruction.source} to #{target_path}")
    end
  end
  def assemble_asset!(instruction, _, for: module) do
    # don't assemble it if it's already there.  This lets
    # us easily rewrite test cases.
    unless File.exists?(instruction.target) do
      # make sure that the target directory path exists.
      instruction.target
      |> Path.dirname
      |> File.mkdir_p!
      # send the file in.
      File.cp!(instruction.source, instruction.target)
      # save it in the code map.
      Module.put_attribute(module, :nif_code_map, [{
        instruction.target,
        Path.relative_to_cwd(instruction.source)
      } | Module.get_attribute(module, :nif_code_map)])
      Logger.debug("copied #{instruction.source} to #{instruction.target}")
    end
  end

  def parse_file(file_path, options) do
    check_options!(options)

    {type, target} = case Path.extname(file_path) do
      ".zig" -> {:zig, Path.join(options[:target_dir], file_path)}
      ".h" -> {:cinclude, {:include, file_path}}
      _ -> raise "unsupported file extension"
    end

    source = Path.join(options[:parent_dir], file_path)

    transitive_imports = source
    |> File.read!
    |> parse_code(options)

    [struct(__MODULE__, options ++
      [type: type, source: source, target: target])]
    ++ transitive_imports
  end

  def parse_code(code, options!) do
    check_options!(options!)

    code
    |> IO.iodata_to_binary
    |> Imports.parse
    |> Enum.reverse
    |> Enum.reject(&standard_components/1)
    |> Enum.reject(&in_cache/1)
    |> Enum.map(&cache_file/1)
    |> Enum.flat_map(&import_to_assembler(&1, options!))
  end

  #####################################################################
  # dependency pruning
  #
  # allows filtering standard components.  These are either in the zig
  # standard libraries or intended to be put into place by the zigler system.

  defp standard_components({:pub, _, _}), do: false
  defp standard_components({_, "erl_nif.zig"}), do: true
  defp standard_components({_, "beam.zig"}), do: true
  defp standard_components({:cinclude, _include}), do: false
  defp standard_components({_, maybe_standard}) do
    Path.extname(maybe_standard) != ".zig"
  end

  # based on the cache

  defp in_cache({:pub, _, file}), do: file in Process.get(:files_so_far, [])
  defp in_cache({_, file}), do: file in Process.get(:files_so_far, [])

  defp cache_file(spec = {:pub, _, file}) do
    cache(file)
    spec
  end
  defp cache_file(spec = {_, file}) do
    cache(file)
    spec
  end

  defp cache(file) do
    Process.put(:files_so_far, [file | Process.get(:files_so_far, [])])
  end

  ####################################################################

  defp import_to_assembler({:pub, context, file}, options) do
    import_to_assembler({context, file}, options, options[:pub])
  end
  defp import_to_assembler({:cinclude, include}, options) do
    [struct(__MODULE__,
      type: :cinclude,
      source: Path.join([options[:parent_dir], "include", include]),
      target: {:cinclude, include}
    )]
  end
  defp import_to_assembler(spec, options) do
    import_to_assembler(spec, options, false)
  end

  defp import_to_assembler({context, file}, options, pub) do
    import_path = options[:parent_dir]
    |> Path.join(file)
    |> Path.expand()

    import_dir = Path.dirname(import_path)
    import_file = Path.basename(import_path)

    target_dir = options[:target_dir]
    |> Path.join(Path.dirname(file))
    |> Path.expand

    file_context = if context == :usingnamespace, do: [], else: [context]

    parse_file(import_file, [
      parent_dir: import_dir,
      target_dir: target_dir,
      context: options[:context] ++ file_context,
      pub: pub])
  end

  #############################################################################
  ## safety and debug features

  defp requires!(options, tag, what) do
    is_nil(options[tag]) and raise "requires #{what}"
  end

  defp check_options!(options) do
    requires!(options, :parent_dir, "parent directory")
    File.dir?(options[:parent_dir]) or raise "parent directory #{options[:parent_dir]} doesn't exist"
    requires!(options, :target_dir, "target directory")
    requires!(options, :pub, "pub tag")
    requires!(options, :context, "context")
  end

end
