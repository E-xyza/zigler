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

exclude_more = List.wrap(if Version.match?(System.version(), "~> 1.19"), do: :on_upgrade)
exclude = ExUnit.configuration()[:exclude] ++ exclude_more

ExUnit.start(exclude: exclude)
