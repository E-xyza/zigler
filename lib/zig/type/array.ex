defmodule Zig.Type.Array do
  alias Zig.Type
  use Type
  import Type, only: :macros

  @derive Inspect
  defstruct [:child, :len]

  @type t :: %__MODULE__{
          child: Type.t(),
          len: non_neg_integer
        }

  def from_json(%{"child" => child, "len" => len}, module) do
    %__MODULE__{child: Type.from_json(child, module), len: len}
  end

  def marshal_param(_, _), do: nil

  def marshal_return(type, opts) do
    if type.child == ~t(u8) and opts[:return] == :list do
      fn arg ->
        quote bind_quoted: [arg: arg] do
          :binary.bin_to_list(arg)
        end
      end
    end
  end

  def param_errors(_, _), do: nil

  def to_string(array), do: "[#{array.len}]#{Kernel.to_string(array.child)}"

  def to_call(array), do: "[#{array.len}]#{Type.to_call(array.child)}"
end
