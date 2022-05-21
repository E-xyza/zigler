defmodule MacOS.NoCI do
  @moduledoc false

  defmacro __using__(_) do
    if match?({:unix, :darwin}, :os.type()) do
      quote do
        @moduletag :no_ci
      end
    end
  end
end
