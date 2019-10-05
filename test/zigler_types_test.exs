defmodule ZiglerTest.ZiglerTypesTest do
  use ExUnit.Case

  defmodule Floats do
    use Zigler, app: :zigler

    ~Z"""
    @nif("compare")
    fn compare(val: f64) bool {
      return val > 3.14;
    }
    """
  end

  test "floating point values can be passed in and bools can be passed out" do
    assert Floats.compare(4.0)
    refute Floats.compare(3.0)
  end

  defmodule StringIn do
    use Zigler, app: :zigler

    ~Z"""
    @nif("string_in")
    fn string_in(val: [*c]u8) c_int {
      return val[0];
    }
    """
  end

  test "we can get back parts of strings" do
    assert StringIn.string_in("hello") == ?h
    assert StringIn.string_in("abc") == ?a
  end

  defmodule SliceIn do
    use Zigler, app: :zigler

    ~Z"""
    @nif("slice_in")
    fn slice_in(env: ?*e.ErlNifEnv, val: []u8) e.ErlNifTerm {

      // build a tuple with the first letter and
      // the slice length.

      var temp = [2]e.ErlNifTerm{
        e.enif_make_int(env, @intCast(c_int, val.len)),
        e.enif_make_int(env, val[0])
      };

      return e.enif_make_tuple_from_array(env, @ptrCast([*c]e.ErlNifTerm, &temp[0]), 2);
    }
    """
  end

  test "we can get back slices" do
    assert {7, ?f} == SliceIn.slice_in("foo-bar")
  end

  defmodule StringOut do
    use Zigler, app: :zigler

    ~Z"""
    @nif("string_out")
    fn string_out(val: c_int) [*c]u8 {

      const source = c"hello";

      // we have to do safe allocations.
      var output = @ptrCast([*c]u8, e.enif_alloc(6));

      //copy hello over to the new binary.  (except first position)
      output[0] = @intCast(u8, val);
      var i: usize = 1;
      while (i < 6){ output[i] = source[i]; i += 1; }

      return output;
    }
    """
  end

  test "we can get out a string" do
    assert StringOut.string_out(?m) == "mello"
  end

  defmodule SliceOut do
    use Zigler, app: :zigler

    ~Z"""
    @nif("slice_out")
    fn slice_out(val: c_int) []u8 {

      const source = "hello123456789";

      // we have to do safe allocations.
      var output: [*c]u8 = @ptrCast([*c]u8, e.enif_alloc(8));

      // and safe copies
      var i: usize = 0;
      while (i < 8){ output[i] = source[i]; i += 1; }

      return output[0..@intCast(usize, val)];
    }
    """
  end

  test "we can get out a slice" do
    assert SliceOut.slice_out(3) == "hel"
  end
end
