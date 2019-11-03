defmodule ZiglerTest.ZiglerTypesTest do
  use ExUnit.Case

  defmodule ZeroArity do
    use Zigler, app: :zigler

    ~Z"""
    /// nif: zeroarity/0
    fn zeroarity() i64 {
      return 47;
    }

    /// nif: zeroarity2/0
    fn zeroarity2(env: beam.env) i64 {
      return 47;
    }
    """
  end

  test "zero arity functions work fine" do
    assert 47 == ZeroArity.zeroarity()
    assert 47 == ZeroArity.zeroarity2()
  end
  defmodule Int64 do
    use Zigler, app: :zigler

    ~Z"""
    /// nif: compare/2
    fn compare(left: i64, right: i64) bool {
      return left < right;
    }
    """
  end

  test "we can pass in i64 types" do
    assert Int64.compare(2, 3)
    refute Int64.compare(6, 2)
  end

  defmodule Floats do
    use Zigler, app: :zigler

    ~Z"""
    /// nif: compare/1
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
    /// nif: string_in/1
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
    /// nif: slice_in/1
    fn slice_in(env: ?*e.ErlNifEnv, val: []u8) e.ErlNifTerm {

      // build a tuple with the first letter and
      // the slice length, this should prove we can send stuff
      // as slices.

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
    /// nif: string_out/1
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
    /// nif: slice_out/1
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

  defmodule TermIn do
    use Zigler, app: :zigler

    ~Z"""
    /// nif: term_in/1
    fn term_in(env: ?*e.ErlNifEnv, msg: e.ErlNifTerm) c_int {

      // we can put an arbitrary term in there; in this case
      // we will send a PID and use it to shoot a message back
      // to the calling test.

      var pid: e.ErlNifPid = undefined;

      var p2 = e.enif_self(env, &pid);
      var res = e.enif_send(env, &pid, env, msg);

      return 47;  // why not.
    }
    """
  end

  test "we can send a term in" do
    assert 47 == TermIn.term_in({:test, "me"})
    assert_receive {:test, "me"}
  end

  defmodule PidIn do
    use Zigler, app: :zigler

    ~Z"""
    /// nif: pid_in/1
    fn pid_in(env: ?*e.ErlNifEnv, pid: e.ErlNifPid) c_int {

      // we can can also send pids directly
      var msg: e.ErlNifTerm = e.enif_make_atom(env, c"ok");

      var res = e.enif_send(env, &pid, env, msg);

      return 47;  // why not.
    }
    """
  end

  test "we can send a pid in" do
    assert 47 == PidIn.pid_in(self())
    assert_receive :ok
  end
end
