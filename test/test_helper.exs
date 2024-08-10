log_level =
  case System.get_env("CI_LOG_LEVEL", "warning") do
    "warning" -> :warning
    "info" -> :info
  end

Logger.configure(level: log_level)

custom_directory = "test/.custom_location"

if File.dir?(custom_directory), do: File.rm_rf!("test/.custom_location")
File.mkdir_p!("test/.custom_location")

ZiglerTest.Compiler.init()

ZiglerTest.MakeGuides.go()

ZiglerTest.MakeBeam.go()

ZiglerTest.MakeReadme.go()

ZiglerTest.MakeZig.go()

ExUnit.start()
