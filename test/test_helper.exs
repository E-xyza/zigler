log_level =
  case System.get_env("CI_LOG_LEVEL", "warning") do
    "warning" -> :warning
    "info" -> :info
  end

Logger.configure(level: log_level)

ZiglerTest.Compiler.init()

ZiglerTest.MakeGuides.go()

ZiglerTest.MakeBeam.go()

ZiglerTest.MakeReadme.go()

ZiglerTest.MakeZig.go()

ExUnit.start()
