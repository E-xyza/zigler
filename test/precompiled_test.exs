require Logger

# all the relevent tests currently have high erts version requirements

this_version =
  case String.split("#{:erlang.system_info(:version)}", ".") do
    [major, minor] -> "#{major}.#{minor}.0"
    [major, minor, patch | _] -> "#{major}.#{minor}.#{patch}"
  end

windows? = :os.type() == {:win32, :nt}

if Version.match?(this_version, ">= 16.1.0") and not windows? do
  priv_dir = :code.priv_dir(:zigler)

  suffix = if windows?, do: "dll", else: "so"

  lib_path = Path.join(priv_dir, "lib/precompiled.#{suffix}")

  if not File.exists?(lib_path) do
    zig_path = Path.join(__DIR__, "precompiled.zig")
    erl_nif_path = Path.join(priv_dir, "beam/erl_nif.zig")
    beam_path = Path.join(priv_dir, "beam/beam.zig")

    system_include_path =
      Path.join([:code.root_dir(), "/erts-#{:erlang.system_info(:version)}", "/include"])

    windows_system_include_path = if windows?, do: "-I#{:code.priv_dir(:zigler)}/erl_nif_win"

    Zig.Command.run_zig(
      """
      build-lib
        -dynamic
        -fallow-shlib-undefined
        -fPIC
        -O ReleaseSafe
        --dep erl_nif
        --dep beam
        -Mroot=#{zig_path}
        -lc
        -I#{system_include_path}
        #{windows_system_include_path}
        -Merl_nif=#{erl_nif_path}
        --dep erl_nif
        -Mbeam=#{beam_path}
        -femit-bin=#{lib_path}
      """,
      []
    )
  end

  File.rm("priv/lib/Elixir.ZiglerTest.LocalPrecompiledTest.#{suffix}")
  File.rm("priv/lib/Elixir.ZiglerTest.WebPrecompiledTest.#{suffix}")
  File.rm("priv/lib/Elixir.ZiglerTest.MultiplatformPrecompiledTest.#{suffix}")

  if :os.type() == {:unix, :linux} do
    File.rm_rf!("/tmp/Elixir.ZiglerTest.WebPrecompiledTest")
  end

  ZiglerTest.LocalPrecompiledTest
  |> Zig.Builder.staging_directory()
  |> File.rm_rf!()

  ZiglerTest.MultiplatformPrecompiledTest
  |> Zig.Builder.staging_directory()
  |> File.rm_rf!()

  Logger.debug("cleared existing assets")

  Process.sleep(100)

  defmodule ZiglerTest.LocalPrecompiledTest do
    use ExUnit.Case, async: true

    use Zig, otp_app: :zigler, precompiled: "./priv/lib/precompiled.#{suffix}"

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

  defmodule ZiglerTest.MultiplatformPrecompiledTest do
    use ExUnit.Case, async: true

    @lib_address "https://github.com/E-xyza/zigler/releases/download/0.15.1/Elixir.ZiglerTest.MultiplatformPrecompiledTest.#VERSION.#TRIPLE.#EXT"

    @shasum [
      "aarch64-freebsd-none": "1102b0c61fa633c4ce2b23abd4c4f238ef44ce9c384b0a9753ff27a541d065d0",
      "aarch64-linux-gnu": "d08ceca0fbd3a00fc0a1a6d012ddab93147aff1ad6d0deb8a9cfd97dbb9c63dc",
      "aarch64-linux-musl": "45012a9f90038938365fc4a4764ca59b7ad1ef3fdf642ebb9d9b24c4dd332ff5",
      "aarch64-macos-none": "bda840e4a036b16154ec10f19b5049d3ba0f9d6b4961af405b21fc9f6556b143",
      "arm-linux-gnueabi": "d7b4941de3ae2147c19a581fe942baaf9ab68fe1bb0a26cb1dd2906a19691dbf",
      "arm-linux-gnueabihf": "dae78e59dd6133dbdf3165a6013b27d53e5becd804e0d06b8d907e3496f8a60f",
      "arm-linux-musleabi": "9063fb1d80e0bc76e884455f4ce621eec4b5c1091d056ffbc9084313589293b4",
      "arm-linux-musleabihf": "f152655bad92d7b697b33391da5a3616569f54d1ed92df31b1d7bad46aa7b281",
      "x86_64-freebsd-none": "3e6e015c7db70ea2835d0baa34c6de57c5606c3068e4be8e3e18e11baa047959",
      "x86_64-linux-gnu": "569501fb9194c1b52828cfe0d0db53c51d3f18c3bc40941e6cc4a5debfce42a3",
      "x86_64-linux-musl": "6fa18747140ac37a80bd51569c2c0de415f9e62d3285524c6fc5c873ec3ddc5b",
      "x86_64-macos-none": "12523525c63d7a5fd504319b9349c788a6a0e138e033b8ea4579779ad674763a",
      "x86_64-windows-msvc": "5eea60eca2ca7f183f06372f214b1066a5ea166dc5ef65a7cd8797b5dd411e95",
      "x86_64-windows-gnu": "dcf44478a6c8dc26a13e0405b7b323a1b19a3f36bcc0fdcedbaa1b590dceb404",
      "x86-linux-gnu": "351f2177a43ea735d1e52474e6059744e4b7c7efd256d618d61e54b2d1989ad4",
      "x86-linux-musl": "7bb33abafa856da069155ed0e9e41fb2fdf0a1cadddced0a4dcf73d96d05c380",
      "x86-windows-gnu": "45834720aacf86153c8335eeb2d124fb25af4ba0daa804c8aa3f23718a5da72c"
    ]

    use Zig, otp_app: :zigler, precompiled: {:web, @lib_address, @shasum}

    ~Z"""
    pub fn add_one(x: u32) u32 {
        return x + 1;
    }
    """

    test "staging directory only contains the so file" do
      assert [
               "Elixir.ZiglerTest.MultiplatformPrecompiledTest.0.15.1" <> _
             ] = File.ls!(Zig.Builder.staging_directory(__MODULE__))
    end

    test "function works" do
      assert add_one(47) == 48
    end
  end
end
