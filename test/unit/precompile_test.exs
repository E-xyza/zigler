defmodule ZiglerTest.PrecompileTest do
  use ExUnit.Case, async: true

  #
  # tests to make sure that various precompile tasks have happened.
  #

  alias Zigler.Module

  @module %Module{file: "foo.ex", module: Foo, otp_app: :zigler}

  describe "when you run your precompile" do
    test "your staging directory is created" do
      compile = Zigler.Compiler.precompile(@module)
      assert compile.assembly_dir =~ "Foo"
      assert File.dir?(compile.assembly_dir)
    end

    test "the code file is created" do
      compile = Zigler.Compiler.precompile(@module)

      assert Path.dirname(compile.code_file) == compile.assembly_dir
      assert Path.basename(compile.code_file) =~ "Foo"
      assert Path.extname(compile.code_file) == ".zig"
      assert File.exists?(compile.code_file)
    end

    test "the code file contains generated code content" do
      compile = Zigler.Compiler.precompile(%{@module | code: "bar"})
      assert File.read!(compile.code_file) =~ "bar"
    end

    test "beam.zig is installed" do
      compile = Zigler.Compiler.precompile(@module)

      assert compile.assembly_dir
      |> Path.join("beam.zig")
      |> File.exists?
    end

    test "erl_nif_zig.h is installed" do
      compile = Zigler.Compiler.precompile(@module)

      assert compile.assembly_dir
      |> Path.join("include/erl_nif_zig.h")
      |> File.exists?
    end
  end
end
