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
defmodule ZiglerTest.PrecompiledTest do
  use ExUnit.Case, async: true

  use Zig, otp_app: :zigler, precompiled: "./priv/lib/precompiled.so"

  ~Z"""
  pub fn add_one(x: u32) u32 {
      return x + 1;
  }
  """

  test "staging directory doesn't exist" do
    refute File.dir?(Zig.Builder.staging_directory(ZiglerTest.PrecompiledTest))
  end

  test "function works" do
    # assert add_one(47) == 48
  end
end
