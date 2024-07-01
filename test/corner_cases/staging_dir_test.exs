defmodule ZiglerTest.CornerCases.StagingDirTest do
  # this test is not async because we can set the staging directory
  # as an environment variable
  use ExUnit.Case

  @staging_dir Path.join(System.tmp_dir!(), "zigler_staging")

  require ZiglerTest.Compiler

  test "specifying staging dir works" do
    System.put_env("ZIGLER_STAGING_ROOT", @staging_dir)

    if File.dir?(@staging_dir) do
      File.rm_rf!(@staging_dir)
    end

    modname = ZiglerTest.Compiler.compile("staging_dir.ex")

    assert @staging_dir
           |> Path.join("#{modname}")
           |> File.dir?()

    File.rm_rf!(@staging_dir)
  end

  test "an invalid staging dir yields a usable error" do
    System.put_env("ZIGLER_STAGING_ROOT", "/this/is/not/a/usable/path")

    assert_raise File.Error,
                 "could not make directory (with -p), consider setting ZIGLER_STAGING_ROOT environment variable\n \"/this/is/not/a/usable/path/Elixir.Zigler.StagingDir.This.Is.Not.A.Usable.Path\": no such file or directory",
                 fn ->
                   ZiglerTest.Compiler.compile("staging_dir.ex")
                 end
  end
end
