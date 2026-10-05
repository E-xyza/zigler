log_level =
  case System.get_env("CI_LOG_LEVEL", "warning") do
    "warning" -> :warning
    "info" -> :info
    "debug" -> :debug
  end

Application.put_env(:zigler, :test_blas, System.get_env("ZIGLER_TEST_BLAS", "FALSE") == "TRUE")

Logger.configure(level: log_level)

custom_directory = "test/.custom_location"

if :os.type() == {:win32, :nt} do
  File.mkdir_p!(System.tmp_dir!())
end

if File.dir?(custom_directory), do: File.rm_rf!("test/.custom_location")
File.mkdir_p!("test/.custom_location")

ZiglerTest.Compiler.init()

ZiglerTest.MakeGuides.go()

ZiglerTest.MakeBeam.go()

ZiglerTest.MakeReadme.go()

ZiglerTest.MakeZig.go()

# Nearly every test here compiles zig, and CI runners are far slower than a
# development machine: the suite takes ~20s locally and ~1600s on the macos
# runner.  The default 60s per-test timeout is comfortable locally but marginal
# there for the tests that compile a nif inside the test body.
ExUnit.start(timeout: String.to_integer(System.get_env("ZIGLER_TEST_TIMEOUT", "300000")))
