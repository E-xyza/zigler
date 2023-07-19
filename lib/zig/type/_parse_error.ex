defmodule Zig.Type.ParseError do
  defexception [:source, :reason]

  def message(%{reason: nil} = error) do
    "the type #{error.source} is not a type usable by zigler"
  end

  def message(error) do
    "the type #{error.source} is not a type usable by zigler (#{error.reason})"
  end
end
