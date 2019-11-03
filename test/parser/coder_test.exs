defmodule ZigTest.CodeTest do
  use ExUnit.Case

  alias Zigler.Code
  alias Zigler.Parser

  test "tokenization" do
    assert Parser.tokenize(1, """
        @nif("hello");
        /* this is a block
           comment that looks like
           what C uses */
        fn hello(val: i64) i64 {
          return 1;
        }
        """)
    |> Code.to_string == ~S"""
    fn hello (val: i64 )i64 {  //line 5
      return 1;
    }
    """
  end
end
