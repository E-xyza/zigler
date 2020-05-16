defmodule Zigler.Assembler do
  @moduledoc """
  Phase One of Zigler compilation process.

  Looks through the contents of the zig code and creates a map
  of required source code files.  Then outputs a struct containing
  the relevant information.
  """

  defstruct [:type, :source, :target, context: []]

  @type file_type :: :zig | :cinclude

  @type t :: %__MODULE__{
    type: file_type,
    source: Path.t,
    target: Path.t,
    context: [String.t],
  }

  def parse(file_path, options) do
    unless options[:parent_dir], do: raise "requires a parent directory"
    unless options[:target_dir], do: raise "requires a target directory"

    {type, target} = case Path.extname(file_path) do
      ".zig" -> {:zig, Path.join(options[:target_dir], file_path)}
      ".h" -> {:cinclude, {:include, file_path}}
      _ -> raise "unsupported file extension"
    end

    source = Path.join(options[:parent_dir], file_path)

    [struct(__MODULE__, type: type, source: source, target: target)]
  end

end
