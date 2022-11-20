defmodule Zig.Type.Cpointer do
  alias Zig.Type
  use Type
  import Type, only: :macros

  defstruct [:child]

  @type t :: %__MODULE__{child: Type.t()}

  def from_json(%{"child" => child}, module) do
    %__MODULE__{child: Type.from_json(child, module)}
  end

  def marshal_param(_, _), do: nil

  def marshal_return(type, opts) do
    #if type.child == ~t(u8) and opts[:return] == :list do
    #  fn arg ->
    #    quote bind_quoted: [arg: arg] do
    #      :binary.bin_to_list(arg)
    #    end
    #  end
    #end
    nil
  end

  def param_errors(type, _opts) do
#    type_str = to_string(type)
#
#    fn index ->
#      [
#        {{:nif_argument_type_error, index},
#         quote do
#           case __STACKTRACE__ do
#             [{_m, _f, a, _opts}, {m, f, _a, opts} | rest] ->
#               item = Enum.at(a, unquote(index))
#
#               msg =
#                 cond do
#                   not is_list(item) ->
#                     "\n\n     expected: list (#{unquote(type_str)})\n     got: #{inspect(item)}"
#
#                   true ->
#                     child_str = unquote(Kernel.to_string(type.child))
#
#                     "\n\n     expected: list (#{unquote(type_str)}) but one of the list items (in #{inspect(item)}) has the wrong type"
#                 end
#
#               new_opts =
#                 Keyword.merge(opts,
#                   error_info: %{module: __MODULE__, function: :_format_error},
#                   zigler_error: %{unquote(index + 1) => msg}
#                 )
#
#               :erlang.raise(:error, :badarg, [{m, f, a, new_opts} | rest])
#
#             stacktrace ->
#               :erlang.raise(:error, :badarg, stacktrace)
#           end
#         end}
#      ]
#    end
    nil
  end

  def to_string(slice), do: "[*c]#{Kernel.to_string(slice.child)}"

  def to_call(slice), do: "[*c]#{Type.to_call(slice.child)}"

  # for now.
  def return_allowed?(pointer), do: false
end
