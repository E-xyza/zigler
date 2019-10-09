# put this in one place to make it easy to configure system-wide
Application.put_env(:zigler, :latest_zig_version, "0.5.0")

# default release modes.
# you can override these in your `use Zigler` statement.
release_mode = %{
  prod: :safe,
  dev: :debug,
  test: :debug}

Application.put_env(:zigler, :release_mode, release_mode[Mix.env()])
