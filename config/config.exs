# default release modes.
# you can override these in your `use Zigler` statement.
release_mode = %{
  prod: :safe,
  dev: :debug,
  test: :debug}

Application.put_env(:zigler, :release_mode, release_mode[Mix.env()])
