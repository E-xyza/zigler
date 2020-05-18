defmodule Zigler.Assembler do
  @moduledoc """
  Phase One of Zigler compilation process.

  Looks through the contents of the zig code and creates a map
  of required source code files.  Then outputs a struct containing
  the relevant information.
  """

  require Logger

  defstruct [:type, :source, :target, context: [], pub: false]

  @type file_type :: :zig | :cinclude

  @type t :: %__MODULE__{
    type: file_type,
    source: Path.t,
    target: Path.t,
    context: [String.t],
    pub: boolean
  }

  alias Zigler.Parser.Imports

  @doc """
  assembles the "core" part of the zig adapter code, these are parts for
  shimming Zig libraries into the BEAM that are going to be @import'd into
  the runtime by default.
  """
  def assemble_kernel!(assembly_dir) do
    # copy in beam.zig
    File.cp!("zig/beam/beam.zig", Path.join(assembly_dir, "beam.zig"))
    # copy in erl_nif.zig
    File.cp!("zig/beam/erl_nif.zig", Path.join(assembly_dir, "erl_nif.zig"))
    # copy in erl_nif_zig.h
    File.mkdir_p!(Path.join(assembly_dir, "include"))
    File.cp!("zig/include/erl_nif_zig.h", Path.join(assembly_dir, "include/erl_nif_zig.h"))
  end

  @doc """
  assembles zig assets, taking them from their source to and putting them into
  the target directory.
  """
  def assemble_assets!(assembly) do
    Enum.each(assembly, fn instruction ->
      # make sure that the target directory path exists.
      instruction.target
      |> Path.dirname
      |> File.mkdir_p!
      # send the file in.
      File.cp!(instruction.source, instruction.target)
      Logger.debug("copied #{instruction.source} to #{instruction.target}")
    end)
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

  # TODO: refactor it so that the import parser itself outputs
  # a datastructure which can be immediately turned around and used.
  def parse_code(code, options) do
    check_options!(options)
    code
    |> IO.iodata_to_binary
    |> Imports.parse
    |> Enum.reverse
    |> Enum.flat_map(&import_to_assembler(&1, options))
  end

  defp import_to_assembler({:pub, context, file}, options) do
    import_to_assembler({context, file}, options, options[:pub])
  end
  defp import_to_assembler({context, file}, options, pub \\ false) do
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
