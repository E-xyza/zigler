defmodule Zig.Type.Struct do
  use Zig.Type

  defstruct [:name]

  def from_json(json = %{"name" => "." <> name}) do
    json |> IO.inspect(label: "lib/zig/type/struct.ex:#{__ENV__.line}")
    %__MODULE__{name: name}
  end

  def marshal_param(_), do: nil

  def marshal_return(_), do: nil

  def param_errors(_), do: nil

  def to_string(struct), do: struct.name

end
