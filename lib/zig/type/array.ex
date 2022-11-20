defmodule Zig.Type.Array do
  alias Zig.Type
  use Type
  import Type, only: :macros

  defstruct [:child, :len, :has_sentinel?, :repr, mutable: false]

  @type t :: %__MODULE__{
          child: Type.t(),
          repr: String.t(),
          len: non_neg_integer,
          mutable: boolean,
          has_sentinel?: boolean
        }

  def from_json(
        %{"child" => child, "len" => len, "hasSentinel" => has_sentinel?, "repr" => repr},
        module
      ) do
    %__MODULE__{
      child: Type.from_json(child, module),
      len: len,
      has_sentinel?: has_sentinel?,
      repr: repr
    }
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

  def param_errors(type, _opts) do
    type_str = to_string(type)

    fn index ->
      [
        {{:nif_argument_type_error, index},
         quote do
           case __STACKTRACE__ do
             [{_m, _f, a, _opts}, {m, f, _a, opts} | rest] ->
               item = Enum.at(a, unquote(index))

               msg =
                 if is_list(item) do
                   child_str = unquote(Kernel.to_string(type.child))

                   "\n\n     expected: list (#{unquote(type_str)}) but one of the list items (in #{inspect(item)}) has the wrong type"
                 else
                   "\n\n     expected: list (#{unquote(type_str)})\n     got: #{inspect(item)}"
                 end

               new_opts =
                 Keyword.merge(opts,
                   error_info: %{module: __MODULE__, function: :_format_error},
                   zigler_error: %{unquote(index + 1) => msg}
                 )

               :erlang.raise(:error, :badarg, [{m, f, a, new_opts} | rest])

             stacktrace ->
               :erlang.raise(:error, :badarg, stacktrace)
           end
         end},
        {{:nif_array_length_error, index},
         quote do
           case __STACKTRACE__ do
             [{_m, _f, a, _opts}, {m, f, _a, opts} | rest] ->
               item = Enum.at(a, unquote(index))

               msg =
                 if is_list(item) do
                   child_str = unquote(Kernel.to_string(type.child))

                   "\n\n     expected: list of length #{unquote(type.len)}\n     got length: #{length(item)}"
                 else
                   "\n\n     expected: binary of size #{unquote(type.len)}\n     got size: #{byte_size(item)}"
                 end

               new_opts =
                 Keyword.merge(opts,
                   error_info: %{module: __MODULE__, function: :_format_error},
                   zigler_error: %{unquote(index + 1) => msg}
                 )

               :erlang.raise(:error, :badarg, [{m, f, a, new_opts} | rest])

             stacktrace ->
               :erlang.raise(:error, :badarg, stacktrace)
           end
         end}
      ]
    end
  end

  def to_string(array = %{mutable: true}), do: "*" <> to_string(%{array | mutable: false})
  def to_string(%{has_sentinel?: true, repr: repr}), do: repr
  def to_string(array), do: "[#{array.len}]#{Kernel.to_string(array.child)}"

  def to_call(array = %{mutable: true}), do: "*" <> to_call(%{array | mutable: false})
  def to_call(%{has_sentinel?: true, repr: repr}), do: repr
  def to_call(array), do: "[#{array.len}]#{Type.to_call(array.child)}"

  def return_allowed?(array), do: Type.return_allowed?(array.child)
end
