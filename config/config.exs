# put this in one place to make it easy to configure system-wide
Application.put_env(:zigler, :latest_zig_version, "0.5.0")

case :os.type() do
  {:unix, :linux} ->
    Application.put_env(:zigler, :os_package, "zig-linux-x86_64-")

  {:unix, :darwin} ->
    Application.put_env(:zigler, :os_package, "zig-macos-x86_64-")

  _ ->
    raise "Your OS/System is not currently supported."
end
