priv_dir = :code.priv_dir(:zigler)
lib_path = Path.join(priv_dir, "lib/precompiled.so")

if not File.exists?(lib_path) do
  zig_path = Path.join(__DIR__, "precompiled.zig")
  erl_nif_path = Path.join(priv_dir, "beam/erl_nif.zig")
  beam_path = Path.join(priv_dir, "beam/beam.zig")

  system_include_path =
    Path.join([:code.root_dir(), "/erts-#{:erlang.system_info(:version)}", "/include"])

  Zig.Command.run_zig(
    """
    build-lib
      -dynamic
      -fPIC
      -O ReleaseSafe
      --dep erl_nif
      --dep beam
      -Mroot=#{zig_path}
      -lc
      -I#{system_include_path}
      -Merl_nif=#{erl_nif_path}
      --dep erl_nif
      -Mbeam=#{beam_path}
      -femit-bin=#{lib_path}
    """,
    []
  )
end

File.rm("priv/lib/Elixir.ZiglerTest.LocalPrecompiledTest.so")
File.rm("priv/lib/Elixir.ZiglerTest.WebPrecompiledTest.so")
if :os.type() == {:unix, :linux} do
  File.rm_rf("/tmp/Elixir.ZiglerTest.WebPrecompiledTest")
end

Process.sleep(100)
defmodule ZiglerTest.LocalPrecompiledTest do
  use ExUnit.Case, async: true

  use Zig, otp_app: :zigler, precompiled: "./priv/lib/precompiled.so"

  ~Z"""
  pub fn add_one(x: u32) u32 {
      return x + 1;
  }
  """

  test "staging directory doesn't exist" do
    refute File.dir?(Zig.Builder.staging_directory(__MODULE__))
  end

  test "function works" do
    assert add_one(47) == 48
  end
end

if :os.type() == {:unix, :linux} do
  defmodule ZiglerTest.WebPrecompiledTest do
    use ExUnit.Case, async: true

    @lib_address "https://github.com/E-xyza/zigler/releases/download/test-artifact/web-precompiled.so"
    @shasum "935f9829d4c0058acba4118c9dc1a98dbdda5c4035e16a7893c33a3aff2caee8"

    use Zig, otp_app: :zigler, precompiled: {:web, @lib_address, @shasum}

    ~Z"""
    pub fn add_one(x: u32) u32 {
        return x + 1;
    }
    """

    test "staging directory only contains the so file" do
      assert File.ls!(Zig.Builder.staging_directory(__MODULE__)) == ["web-precompiled.so"]
    end

    test "function works" do
      assert add_one(47) == 48
    end
  end
end
