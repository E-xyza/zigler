defmodule ZiglerTest.Snapshot.ImportsTest do
  use ExUnit.Case, async: true

  describe "c_imports" do
    test "generates the default correctly" do
      assert ["""
      const e = @cImport({
        @cInclude("erl_nif_zig.h");
      });
      """] == Zigler.Zig.c_imports([e: "erl_nif_zig.h"])
    end

    test "generates arbitrary content correctly" do
      assert ["""
      const foo = @cImport({
        @cInclude("bar.h");
      });
      """] == Zigler.Zig.c_imports([foo: "bar.h"])
    end

    test "generates multiple includes in the same namespace correctly" do
      assert ["""
      const foo = @cImport({
        @cInclude("bar.h");
        @cInclude("baz.h");
      });
      """] == Zigler.Zig.c_imports([foo: ["bar.h", "baz.h"]])
    end

    test "generates multiple includes in the same namespace, specified separately, correctly" do
      assert ["""
      const foo = @cImport({
        @cInclude("bar.h");
        @cInclude("baz.h");
      });
      """] == Zigler.Zig.c_imports([foo: "bar.h", foo: "baz.h"])
    end

    test "can generate multiple namespaces" do
      assert """
      const foo = @cImport({
        @cInclude("bar.h");
      });
      const baz = @cImport({
        @cInclude("quux.h");
      });
      """ == Zigler.Zig.c_imports([foo: "bar.h", baz: "quux.h"]) |> IO.iodata_to_binary
    end
  end

end
