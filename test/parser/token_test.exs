defmodule Parser.TokenTest do

  #makes sure that we can parse tokens accurately.
  use ExUnit.Case

  alias Zigler.Parser

  describe "the tokenizer can identify" do
    test "basic types" do
      # integers
      assert [:i8] == Parser.tokenize(4, "i8")
      assert [:i16] == Parser.tokenize(3, "i16")
      assert [:i32] == Parser.tokenize(3, "i32")
      assert [:i64] == Parser.tokenize(3, "i64")

      # floats
      assert [:f16] == Parser.tokenize(1, "f16")
      assert [:f32] == Parser.tokenize(1, "f32")
      assert [:f64] == Parser.tokenize(1, "f64")
    end

    test "array types" do
      assert [slice: :f16] == Parser.tokenize(8, "[]f16")
      assert [slice: :i64] == Parser.tokenize(8, "[]i64")

      assert [:string]     == Parser.tokenize(10, "[]u8")
      assert [:cstring]    == Parser.tokenize(10, "[*c]u8")
    end
  end

  describe "the tokenizer can tokenize" do
    test "an entire segment" do
      assert Parser.tokenize(1, """
        const assert = @import("std").debug.assert;

        @nif("one")
        fn one() i64 {
          return 1;
        }

        test "the truth" {
          assert(one() + 1 == 2);
        }
      """) == ["const", "assert", "=", :import, [string: "std"], :., "debug", :.,
      "assert", {:line, 1}, :nif, [string: "one"], :fn, "one", [], :i64, {:block, 4,
      "\n    return 1;\n  "}, :test, {:string, "the truth"}, {:block, 8,
      "\n    assert(one() + 1 == 2);\n  "}]
    end

    test "function headers are correctly supported" do
      assert Parser.tokenize(1, """
      fn abc(val: i64) i64 {
        return val + 1;
      }
      """) == [:fn, "abc", ["val:", :i64], :i64, {:block, 1, "\n  return val + 1;\n"}]
    end
  end

  describe "when encountering multiline string literals" do
    test "the parser interprets them correctly" do

    end
  end

  describe "when encountering comments" do
    test "the parser ignores double slashes" do
      assert Parser.tokenize(1, """
      // this is a comment so
      // nothing should show up.
      """) == []

      assert Parser.tokenize(1, """
      // comments don't mess up
      const statement = 5;
      """) == ["const", "statement", "=", "5", {:line, 2}]
    end

    test "and respects docstrings" do
      assert [{:docstring, " this is a docstring\n so get it right.\n"},
        :fn, "test_fn", [], :i64, {:block, 3, _}] =
        Parser.tokenize(1, """
      /// this is a docstring
      /// so get it right.
      fn test_fn() i64 {
        return 1;
      }
      """)
    end

    test "and understands block comment" do
      assert [:nif, [string: "hello"], {:line, 1}, :fn, "hello", ["val:", :i64], :i64,
      {:block, 5, _}] = Parser.tokenize(1, """
      @nif("hello");
      /* this is a block
         comment that looks like
         what C uses */
      fn hello(val: i64) i64 {
        return 1;
      }
      """)
    end
  end
end
