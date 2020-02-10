defmodule ZiglerTest.Snapshot.ImportsTest do
  use ExUnit.Case, async: true

  alias Zigler.Code

  # tests header conditions generated from the module struct's c_includes and
  # imports fields.

  describe "Code.c_imports/1" do
    test "generates the default correctly" do
      assert ["""
      const e = @cImport({
        @cInclude("erl_nif_zig.h");
      });
      """] == Code.c_imports([e: "erl_nif_zig.h"])
    end

    test "generates arbitrary content correctly" do
      assert ["""
      const foo = @cImport({
        @cInclude("bar.h");
      });
      """] == Code.c_imports([foo: "bar.h"])
    end

    test "generates multiple includes in the same namespace correctly" do
      assert ["""
      const foo = @cImport({
        @cInclude("bar.h");
        @cInclude("baz.h");
      });
      """] == Code.c_imports([foo: ["bar.h", "baz.h"]])
    end

    test "generates multiple includes in the same namespace, specified separately, correctly" do
      assert ["""
      const foo = @cImport({
        @cInclude("bar.h");
        @cInclude("baz.h");
      });
      """] == Code.c_imports([foo: "bar.h", foo: "baz.h"])
    end

    test "can generate multiple namespaces" do
      assert """
      const foo = @cImport({
        @cInclude("bar.h");
      });
      const baz = @cImport({
        @cInclude("quux.h");
      });
      """ == Code.c_imports([foo: "bar.h", baz: "quux.h"]) |> IO.iodata_to_binary
    end
  end

  describe "Code.includes/1" do
    test "generates the default correctly" do
      assert """
      const e = @import("erl_nif.zig").c;
      const builtin = @import("builtin");
      const std = @import("std");
      const beam = @import("beam.zig");
      """ == Code.zig_imports([builtin: "builtin", std: "std", beam: "beam.zig"])
        |> IO.iodata_to_binary
    end

    test "generates an arbitrary list correctly" do
      assert """
      const e = @import("erl_nif.zig").c;
      const foo = @import("bar");
      """ == Code.zig_imports([foo: "bar"])
        |> IO.iodata_to_binary
    end
  end
end
