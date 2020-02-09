defmodule ZiglerTest.PrecompileTest do
  use ExUnit.Case, async: true

  #
  # tests to make sure that various precompile tasks have happened.
  #

  describe "when you run your precompile" do
    test "your staging directory is created" do
      compile = Zigler.Compiler.precompile(%{module: Foo})
      assert compile.staging_dir =~ "Foo"
      assert File.dir?(compile.staging_dir)
    end

    test "the code file is created" do
      compile = Zigler.Compiler.precompile(%{module: Foo})

      assert Path.dirname(compile.code_file) == compile.staging_dir
      assert Path.basename(compile.code_file) =~ "Foo"
      assert Path.extname(compile.code_file) == ".zig"
      assert File.exists?(compile.code_file)
    end
  end
end
