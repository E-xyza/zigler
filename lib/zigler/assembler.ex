defmodule Zigler.Assembler do
  @moduledoc """
  Phase One of Zigler compilation process.

  Looks through the contents of the zig code and creates a map
  of required source code files.  Then outputs a struct containing
  the relevant information.
  """

  defstruct [:type, :source, :target, context: [], pub: false]

  @type file_type :: :zig | :cinclude

  @type t :: %__MODULE__{
    type: file_type,
    source: Path.t,
    target: Path.t,
    context: [String.t],
    pub: boolean
  }

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
    |> Zigler.Parser.Imports.parse
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
    requires!(options, :target_dir, "target directory")
    requires!(options, :pub, "pub tag")
    requires!(options, :context, "context")
  end

end
