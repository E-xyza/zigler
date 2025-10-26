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

  defmodule ZiglerTest.MultiplatformPrecompiledTest do
    use ExUnit.Case, async: true

    @lib_address "https://github.com/E-xyza/zigler/releases/download/0.15.1/Elixir.ZiglerTest.MultiplatformPrecompiledTest.#VERSION.#TRIPLE.#EXT"

    @shasum [
      "aarch64-freebsd-none": "e7b413de8bbf37876773dac05f3410f0bf3cc80f99d897557b6819e8189fb006",
      "aarch64-linux-gnu": "17546c34adf8b6a14dd38ebb9d5485610348e5afff72e88b991f18d6b818197f",
      "aarch64-linux-musl": "4ccae1cb87fbef01a556b7e8834da256a5dfa6f8a68d57d95005680ce4e37e16",
      "aarch64-macos-none": "5006c503b8b5d4efff875a2979f6aab7e1cc3c0360fa8618343157964aec1d15",
      "arm-linux-gnueabi": "4e56cd1447ba3b565756d7298a6d89e8cbff13966a27c56fce5a72ad23a9ea4d",
      "arm-linux-gnueabihf": "2355e863c21c1120de0776c98b3ab268e6d633ee3e48d47a6477c9a4a4efee58",
      "arm-linux-musleabi": "5de29e68005eddf6fa31d6f10532de094071fef89f9ffc3eb701e0d0ea550ff3",
      "arm-linux-musleabihf": "28bc7c872d21eb8524bb2c35b502f4f42f10f0f05c2bc4420b45142e51224b35",
      "x86_64-freebsd-none": "a7ff7317461d0a43ffac60c131320c3e5e005f0e75f9562bb710718c476c85fc",
      "x86_64-linux-gnu": "e38e1dfdd85d711f85e0ecbb42fc34e102cd9dc37079c200e2075cb894acad7c",
      "x86_64-linux-musl": "857b0ed35c78b588fa43dc4a5c4e3dd9143d9102c559a67812b0af6185eae117",
      "x86_64-macos-none": "e5ff347c2d11b463b5e6f0097687d578c800e7fbb725aa33a2e802d3f473371c",
      "x86_64-windows-gnu": "3ee18aec252eca92f19d920f6de143e59700e0f038916fb3717aa9869b2c5102",
      "x86_64-windows-msvc": "9facf75d545b6cf45d2367f42c8a4b0bbea5b57bea72eac83f9a694aa4370e7d",
      "x86-linux-gnu": "9ef68f25143827e6cbea13fa264f0002d7d1f2fc2ad7c07febe5be5fbc4f75c6",
      "x86-linux-musl": "227afab8c4a20b4dc88ae0efeb0f6a93952c9ad54aa3165d166cebd66ae5d158",
      "x86-windows-gnu": "4602d68145becb66d4f357ab163a5cb8a7f5466a4004cd6419f998c241b52d2b"
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
