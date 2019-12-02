defmodule ZiglerTest.Types.EgressTest do
  # this module tests that it's possible to ingress values into zig safely
  # and the zigler assembler correctly infers types for you.

  use ExUnit.Case, async: true
  use Zigler, app: :zigler

  ~Z"""
  /// nif: u8_out/0
  fn u8_out() u8 {
    return 47;
  }

  /// nif: c_int_out/0
  fn c_int_out() c_int {
    return 47;
  }

  /// nif: c_long_out/0
  fn c_long_out() c_long {
    return 47;
  }

  /// nif: i32_out/0
  fn i32_out() i32 {
    return 47;
  }

  /// nif: i64_out/0
  fn i64_out() i64 {
    return 47;
  }

  /// nif: f32_out/0
  fn f32_out() f32 {
    return 47.0;
  }

  /// nif: f64_out/0
  fn f64_out() f64 {
    return 47.0;
  }

  /// nif: u8_out_from_slice/0
  fn u8_out_from_slice() []u8 {
    const src = "foo"[0..];
    var res = beam.allocator.alloc(u8, src.len) catch unreachable;
    for (src) | item, idx | {
      res[idx] = item;
    }
    return res;
  }

  /// nif: u8_out_from_c_string/0
  fn u8_out_from_c_string() [*c]u8 {
    const src = "foo"[0..];
    var cstr = beam.allocator.alloc(u8, 4) catch unreachable;
    var res: [*c]u8 = @ptrCast([*c]u8, &cstr[0]);
    for (src) | item, idx | {
      res[idx] = item;
    }
    res[3] = 0;
    return res;
  }

  /// nif: list_out/0
  fn list_out(env: beam.env) []beam.term {
    var outlist = beam.allocator.alloc(beam.term, 2) catch unreachable;
    outlist[0] = beam.make_atom(env, "foo"[0..]);
    outlist[1] = beam.make_atom(env, "bar"[0..]);
    return outlist;
  }

  /// nif: c_int_list_out/0
  fn c_int_list_out() []c_int {
    var outlist = beam.allocator.alloc(c_int, 3) catch unreachable;
    outlist[0] = 1;
    outlist[1] = 2;
    outlist[2] = 3;
    return outlist;
  }

  /// nif: c_long_list_out/0
  fn c_long_list_out() []c_long {
    var outlist = beam.allocator.alloc(c_long, 3) catch unreachable;
    outlist[0] = 1;
    outlist[1] = 2;
    outlist[2] = 3;
    return outlist;
  }

  /// nif: i32_list_out/0
  fn i32_list_out() []i32 {
    var outlist = beam.allocator.alloc(i32, 3) catch unreachable;
    outlist[0] = 1;
    outlist[1] = 2;
    outlist[2] = 3;
    return outlist;
  }

  /// nif: i64_list_out/0
  fn i64_list_out() []i64 {
    var outlist = beam.allocator.alloc(i64, 3) catch unreachable;
    outlist[0] = 1;
    outlist[1] = 2;
    outlist[2] = 3;
    return outlist;
  }

  /// nif: f16_list_out/0
  fn f16_list_out() []f16 {
    var outlist = beam.allocator.alloc(f16, 3) catch unreachable;
    outlist[0] = 1.0;
    outlist[1] = 2.0;
    outlist[2] = 3.0;
    return outlist;
  }

  /// nif: f32_list_out/0
  fn f32_list_out() []f32 {
    var outlist = beam.allocator.alloc(f32, 3) catch unreachable;
    outlist[0] = 1.0;
    outlist[1] = 2.0;
    outlist[2] = 3.0;
    return outlist;
  }

  /// nif: f64_list_out/0
  fn f64_list_out() []f64 {
    var outlist = beam.allocator.alloc(f64, 3) catch unreachable;
    outlist[0] = 1.0;
    outlist[1] = 2.0;
    outlist[2] = 3.0;
    return outlist;
  }

  /// nif: true_out/0
  fn true_out() bool {
    return true;
  }

  /// nif: false_out/0
  fn false_out() bool {
    return false;
  }
  """

  describe "characters can be egressed" do
    test "correctly" do
      assert 47 == u8_out()
    end
  end

  describe "integers can be egressed" do
    test "c_int" do
      assert 47 == c_int_out()
    end

    test "c_long" do
      assert 47 == c_long_out()
    end

    test "i32" do
      assert 47 == i32_out()
    end

    test "i64" do
      assert 47 == i64_out()
    end
  end

  describe "floats can be egressed" do
    test "f32" do
      assert 47.0 == f32_out()
    end

    test "f64" do
      assert 47.0 == f64_out()
    end
  end

  describe "strings can be egressed" do
    test "from a slice" do
      assert "foo" == u8_out_from_slice()
    end
    test "from a c_string" do
      assert "foo" == u8_out_from_c_string()
    end
  end

  describe "lists can be egressed" do
    test "from internal literals" do
      assert [:foo, :bar] == list_out()
    end

    test "from c_int lists" do
      assert [1, 2, 3] == c_int_list_out()
    end

    test "from c_long lists" do
      assert [1, 2, 3] == c_long_list_out()
    end

    test "from i32 lists" do
      assert [1, 2, 3] == i32_list_out()
    end

    test "from i64 lists" do
      assert [1, 2, 3] == i64_list_out()
    end

    test "from f16 lists" do
      assert [1.0, 2.0, 3.0] == f16_list_out()
    end

    test "from f32 lists" do
      assert [1.0, 2.0, 3.0] == f32_list_out()
    end

    test "from f64 lists" do
      assert [1.0, 2.0, 3.0] == f64_list_out()
    end
  end

  describe "bools can be exported" do
    test "correctly" do
      assert true_out() && is_boolean(true_out())
      assert is_boolean(false_out())
      refute false_out()
    end
  end

end
