defmodule ZiglerTest.PrecompileTest do
  use ExUnit.Case, async: true

  #
  # tests to make sure that various precompile tasks have happened.
  #

  alias Zigler.Module

  @module %Module{file: "foo.ex", module: Foo}

  describe "when you run your precompile" do
    test "your staging directory is created" do
      compile = Zigler.Compiler.precompile(@module)
      assert compile.staging_dir =~ "Foo"
      assert File.dir?(compile.staging_dir)
    end

    test "the code file is created" do
      compile = Zigler.Compiler.precompile(@module)

      assert Path.dirname(compile.code_file) == compile.staging_dir
      assert Path.basename(compile.code_file) =~ "Foo"
      assert Path.extname(compile.code_file) == ".zig"
      assert File.exists?(compile.code_file)
    end

    test "the code file contains generated code content" do
      compile = Zigler.Compiler.precompile(%{@module | code: "bar"})
      assert File.read!(compile.code_file) =~ "bar"
    end
  end
end
