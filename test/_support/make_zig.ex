defmodule ZiglerTest.MakeZig do
  defstruct elixir: [], zig: %{}

  alias Zig.Command

  def go do
    # Read the readme document
    content =
      "lib/zig.ex"
      |> File.read!()
      |> Command.split_on_newline()
      |> Enum.map(&String.trim/1)
      # deescape escaped quotes
      |> Enum.map(&String.replace(&1, ~S(\"), ~S(")))
      |> Enum.reduce({nil, %__MODULE__{}}, fn
        "```" <> _, {Elixir, so_far} ->
          {nil, so_far}

        "```" <> _, {Zig, so_far, _} ->
          {nil, so_far}

        line, {Elixir, so_far} ->
          {Elixir, Map.update!(so_far, :elixir, &add_line(&1, line))}

        "```elixir" <> _, {nil, so_far} ->
          {Elixir, so_far}

        "```zig" <> _, {nil, so_far} ->
          {Zig, so_far, nil}

        ~S(\\\\) <> filename, {Zig, so_far, nil} ->
          filename = String.trim(filename)
          new_zig = Map.put(so_far.zig, filename, "")
          {Zig, %{so_far | zig: new_zig}, filename}

        line, {Zig, so_far, filename} ->
          new_zig = Map.update!(so_far.zig, filename, &add_line(&1, line))
          {Zig, %{so_far | zig: new_zig}, filename}

        _, state ->
          state
      end)
      |> elem(1)

    Enum.each(content.zig, fn {filename, content} ->
      "test"
      |> Path.join(filename)
      |> File.write!(content)
    end)

    templated = """
    defmodule ZiglerTest.ZigTest do
      use ExUnit.Case

    #{content.elixir}
      
    end
    """

    File.write!("test/.zig_test.exs", templated)
  end

  defp add_line(so_far, line) do
    [so_far, line, "\n"]
  end
end
