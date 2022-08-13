defmodule ZiglerTest.Sema.FunctionTest do
  use ExUnit.Case, async: true

  alias Zig.Type
  alias Zig.Sema

  import Type, only: :macros

  defp random do
    16
    |> :crypto.strong_rand_bytes
    |> Base.encode16
  end

  defp build(content) do
    path = Path.join([System.tmp_dir!(), "zig-test-sema-" <> random()])
    File.mkdir_p!(path)

    zig_file = Path.join(path, "basic.zig")
    File.write!(zig_file, content)
    zig_file
  end

  # basic test on semantic analysis
  @basic """
  pub fn basic(x: u8) u8 {
    return x + 1;
  }
  """

  test "a basic function can be found" do
    basic = build(@basic)

    assert [%Type.Function{
      name: :basic,
      arity: 1,
      params: [~t(u8)],
      return: ~t(u8),
    }] = Zig.Sema.analyze_file!(basic, nifs: [:basic])
  end
end
