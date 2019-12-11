defmodule ZiglerTest.Types.IngressTest do
  # this module tests that it's possible to ingress values into zig safely
  # and the zigler assembler correctly infers types for you.

  use ExUnit.Case, async: true
  use Zigler, app: :zigler

  ~Z"""
  /// nif: u8_in/1
  fn u8_in(val: u8) i64 {
    return @intCast(i64, val);
  }

  /// nif: c_int_in/1
  fn c_int_in(val: c_int) i64 {
    return @intCast(i64, val);
  }

  /// nif: c_long_in/1
  fn c_long_in(val: c_long) i64 {
    return @intCast(i64, val);
  }

  /// nif: usize_in/1
  fn usize_in(val: usize) i64 {
    return @intCast(i64, val);
  }

  /// nif: isize_in/1
  fn isize_in(val: isize) i64 {
    return @intCast(i64, val);
  }

  /// nif: i32_in/1
  fn i32_in(val: i32) i64 {
    return @intCast(i64, val);
  }

  /// nif: i64_in/1
  fn i64_in(val: i64) i64 {
    return val - 5;
  }

  /// nif: f16_in/1
  fn f16_in(val: f16) f16 {
    return val - 0.5;
  }

  /// nif: f32_in/1
  fn f32_in(val: f32) f32 {
    return val - 0.5;
  }

  /// nif: f64_in/1
  fn f64_in(val: f64) f64 {
    return val - 0.5;
  }

  /// nif: atom_in/1
  fn atom_in(env: beam.env, val: beam.atom) []u8 {
    // NB this is generally bad code because it incurs a memory leak!
    return beam.get_atom_slice(env, val) catch unreachable;
  }

  /// nif: binary_into_slice/1
  fn binary_into_slice(val: []u8) i64 {
    return @intCast(i64, val.len);
  }

  /// nif: binary_into_c_string/1
  fn binary_into_c_string(val: [*c]u8) []u8 {
    // NB this is generally a bad function since it could incur a segfault.
    return val[0..2];
  }

  /// nif: binary_into_binary/1
  fn binary_into_binary(env: beam.env, bin: beam.binary) i64 {
    return @intCast(i64, bin.size);
  }

  /// nif: binary_into_erl_nif_binary/1
  fn binary_into_erl_nif_binary(env: beam.env, bin: e.ErlNifBinary) i64 {
    return @intCast(i64, bin.size);
  }

  /// nif: pid_in_with_beam/1
  fn pid_in_with_beam(env: beam.env, val: beam.pid) i64 {
    var term = beam.make_atom(env, "foo"[0..]);
    var res = e.enif_send(env, &val, env, term);
    return 47;
  }

  /// nif: pid_in_with_erl_nif/1
  fn pid_in_with_erl_nif(env: beam.env, val: e.ErlNifPid) i64 {
    return pid_in_with_beam(env, val);
  }

  /// nif: i32_list_in/1
  fn i32_list_in(val: []i32) i64 {
    var total: i64 = 0;
    for (val) |item| {
      total += @intCast(i64, item);
    }
    return total;
  }

  /// nif: i64_list_in/1
  fn i64_list_in(val: []i64) i64 {
    var total: i64 = 0;
    for (val) |item| {
      total += item;
    }
    return total;
  }

  /// nif: f16_list_in/1
  fn f16_list_in(val: []f16) f64 {
    var total: f64 = 0.0;
    for (val) |item| {
      total += @floatCast(f64, item);
    }
    return total;
  }

  /// nif: f32_list_in/1
  fn f32_list_in(val: []f32) f64 {
    var total: f64 = 0.0;
    for (val) |item| {
      total += @floatCast(f64, item);
    }
    return total;
  }

  /// nif: f64_list_in/1
  fn f64_list_in(val: []f64) f64 {
    var total: f64 = 0.0;
    for (val) |item| {
      total += item;
    }
    return total;
  }

  /// nif: bool_in/1
  fn bool_in(val: bool) i64 {
    if (val) {
      return 1;
    } else {
      return 0;
    }
  }
  """

  describe "characters can be ingressed" do
    test "correctly" do
      assert 97 == u8_in(?a)
    end

    test "and are guarded for invalid values" do
      assert_raise FunctionClauseError, fn -> u8_in(-1) end
      assert_raise FunctionClauseError, fn -> u8_in(256) end
      assert_raise FunctionClauseError, fn -> u8_in(:atom) end
    end
  end

  describe "c integers can be ingressed" do
    test "correctly" do
      assert 47 == c_int_in(47)
    end

    test "and are guarded for invalid values" do
      assert_raise FunctionClauseError, fn -> c_int_in(:atom) end
      assert_raise FunctionClauseError, fn -> c_int_in(0.47) end
    end
  end

  describe "c longs can be ingressed" do
    test "correctly" do
      assert 47 == c_long_in(47)
    end

    test "and are guarded for invalid values" do
      assert_raise FunctionClauseError, fn -> c_long_in(:atom) end
      assert_raise FunctionClauseError, fn -> c_long_in(0.47) end
    end
  end

  describe "usize can be ingressed" do
    test "correctly" do
      assert 47 == usize_in(47)
    end

    test "and are guarded for invalid values" do
      assert_raise FunctionClauseError, fn -> usize_in(:atom) end
      assert_raise FunctionClauseError, fn -> usize_in(0.47) end
    end
  end

  describe "isize can be ingressed" do
    test "correctly" do
      assert 47 == isize_in(47)
    end

    test "and are guarded for invalid values" do
      assert_raise FunctionClauseError, fn -> isize_in(:atom) end
      assert_raise FunctionClauseError, fn -> isize_in(0.47) end
    end
  end

  describe "32-bit integers can be ingressed" do
    test "correctly" do
      assert 47 == i32_in(47)
    end

    test "and are guarded for invalid values" do
      assert_raise FunctionClauseError, fn -> i32_in(:atom) end
      assert_raise FunctionClauseError, fn -> i32_in(0.47) end
    end
  end

  describe "64-bit integers can be ingressed" do
    test "correctly" do
      assert 42 == i64_in(47)
    end

    test "and are guarded for invalid values" do
      assert_raise FunctionClauseError, fn -> i64_in(:atom) end
      assert_raise FunctionClauseError, fn -> i64_in(0.47) end
    end
  end

  describe "16-bit floats can be ingressed" do
    test "correctly" do
      assert 0.5 == f16_in(1.0)
    end

    test "and are guarded for invalid values" do
      assert_raise FunctionClauseError, fn -> f16_in(:atom) end
      assert_raise FunctionClauseError, fn -> f16_in(2) end
    end
  end

  describe "32-bit floats can be ingressed" do
    test "correctly" do
      assert 0.5 == f32_in(1.0)
    end

    test "and are guarded for invalid values" do
      assert_raise FunctionClauseError, fn -> f32_in(:atom) end
      assert_raise FunctionClauseError, fn -> f32_in(2) end
    end
  end

  describe "64-bit floats can be ingressed" do
    test "correctly" do
      assert 0.5 == f64_in(1.0)
    end

    test "and are guarded for invalid values" do
      assert_raise FunctionClauseError, fn -> f64_in(:atom) end
      assert_raise FunctionClauseError, fn -> f64_in(2) end
    end
  end

  describe "atoms can be ingressed" do
    test "correctly" do
      assert "foo" == atom_in(:foo)
    end

    test "and are guarded for invalid values" do
      assert_raise FunctionClauseError, fn -> atom_in(47) end
      assert_raise FunctionClauseError, fn -> atom_in(0.47) end
      assert_raise FunctionClauseError, fn -> atom_in([]) end
    end
  end

  describe "binaries can be ingressed as slices" do
    test "correctly" do
      assert 3 == binary_into_slice("foo")
    end

    test "and are guarded for invalid values" do
      assert_raise FunctionClauseError, fn -> binary_into_slice(47) end
      assert_raise FunctionClauseError, fn -> binary_into_slice(:foo) end
    end
  end

  describe "binaries can be ingressed as c_strings" do
    test "correctly" do
      assert "fo" == binary_into_c_string("foo")
    end

    test "and are guarded for invalid values" do
      assert_raise FunctionClauseError, fn -> binary_into_c_string(47) end
      assert_raise FunctionClauseError, fn -> binary_into_c_string(:foo) end
    end
  end

  describe "binaries can be ingressed as binary structs" do
    test "correctly" do
      assert 3 == binary_into_binary("foo")
    end

    test "and are guarded for invalid values" do
      assert_raise FunctionClauseError, fn -> binary_into_binary(47) end
      assert_raise FunctionClauseError, fn -> binary_into_binary(:foo) end
    end
  end

  describe "binaries can be ingressed as binary structs via e.ErlNifBinary" do
    test "correctly" do
      assert 3 == binary_into_erl_nif_binary("foo")
    end

    test "and are guarded for invalid values" do
      assert_raise FunctionClauseError, fn -> binary_into_erl_nif_binary(47) end
      assert_raise FunctionClauseError, fn -> binary_into_erl_nif_binary(:foo) end
    end
  end

  describe "pids can be ingressed as beam.pid" do
    test "correctly" do
      assert 47 == pid_in_with_beam(self())
      assert_receive :foo
    end

    test "and are guarded for invalid values" do
      assert_raise FunctionClauseError, fn -> pid_in_with_beam(47) end
      assert_raise FunctionClauseError, fn -> pid_in_with_beam(:foo) end
    end
  end

  describe "pids can be ingressed as e.ErlNifPid" do
    test "correctly" do
      assert 47 == pid_in_with_erl_nif(self())
      assert_receive :foo
    end

    test "and are guarded for invalid values" do
      assert_raise FunctionClauseError, fn -> pid_in_with_erl_nif(47) end
      assert_raise FunctionClauseError, fn -> pid_in_with_erl_nif(:foo) end
    end
  end

  describe "i32 lists can be ingressed" do
    test "correctly" do
      assert 6 == i32_list_in([1, 2, 3])
    end

    test "even with empties" do
      assert 0 == i32_list_in([])
    end

    test "and are guarded for invalid values" do
      assert_raise FunctionClauseError, fn -> i32_list_in(47) end
      assert_raise FunctionClauseError, fn -> i32_list_in(:foo) end
      assert_raise FunctionClauseError, fn -> i32_list_in([22, :foo]) end
    end
  end

  describe "i64 lists can be ingressed" do
    test "correctly" do
      assert 6 == i64_list_in([1, 2, 3])
    end

    test "even with empties" do
      assert 0 == i64_list_in([])
    end

    test "and are guarded for invalid values" do
      assert_raise FunctionClauseError, fn -> i64_list_in(47) end
      assert_raise FunctionClauseError, fn -> i64_list_in(:foo) end
      assert_raise FunctionClauseError, fn -> i64_list_in([22, :foo]) end
      assert_raise FunctionClauseError, fn -> i64_list_in([22, 0.47]) end
    end
  end

  describe "f16 lists can be ingressed" do
    test "correctly" do
      assert 6.0 == f16_list_in([1.0, 2.0, 3.0])
    end

    test "even with empties" do
      assert 0.0 == f16_list_in([])
    end

    test "and are guarded for invalid values" do
      assert_raise FunctionClauseError, fn -> f16_list_in(47.0) end
      assert_raise FunctionClauseError, fn -> f16_list_in(:foo) end
      assert_raise FunctionClauseError, fn -> f16_list_in([40.0, :foo]) end
      assert_raise FunctionClauseError, fn -> f16_list_in([40.0, 3]) end
    end
  end

  describe "f32 lists can be ingressed" do
    test "correctly" do
      assert 6.0 == f32_list_in([1.0, 2.0, 3.0])
    end

    test "even with empties" do
      assert 0.0 == f32_list_in([])
    end

    test "and are guarded for invalid values" do
      assert_raise FunctionClauseError, fn -> f32_list_in(47.0) end
      assert_raise FunctionClauseError, fn -> f32_list_in(:foo) end
      assert_raise FunctionClauseError, fn -> f32_list_in([40.0, :foo]) end
      assert_raise FunctionClauseError, fn -> f32_list_in([40.0, 3]) end
    end
  end

  describe "f64 lists can be ingressed" do
    test "correctly" do
      assert 6.0 == f64_list_in([1.0, 2.0, 3.0])
    end

    test "even with empties" do
      assert 0.0 == f64_list_in([])
    end

    test "and are guarded for invalid values" do
      assert_raise FunctionClauseError, fn -> f64_list_in(47.0) end
      assert_raise FunctionClauseError, fn -> f64_list_in(:foo) end
      assert_raise FunctionClauseError, fn -> f64_list_in([40.0, :foo]) end
      assert_raise FunctionClauseError, fn -> f64_list_in([40.0, 3]) end
    end
  end

  describe "bools can be ingressed" do
    test "correctly" do
      assert 1 == bool_in(true)
      assert 0 == bool_in(false)
    end

    test "and are guarded for invalid values" do
      assert_raise FunctionClauseError, fn -> bool_in(47.0) end
      assert_raise FunctionClauseError, fn -> bool_in(:foo) end
      assert_raise FunctionClauseError, fn -> bool_in([]) end
    end
  end
end
