defmodule ZiglerTest.SlicesTest do
  use ExUnit.Case

  defmodule Int64SliceIn do
    use Zigler, app: :zigler

    ~Z"""
    @nif("sum")
    fn sum(slice: []i64) i64 {
      var total: i64 = 0;
      var index: usize = 0;

      while (index < slice.len) {
        total += slice[index];
        index += 1;
      }

      return total;
    }
    """
  end

  test "we can pass in i64 slice type" do
    assert 6 == Int64SliceIn.sum([1, 2, 3])
    assert 10 == Int64SliceIn.sum([1, 2, 3, 4])
    assert 0 == Int64SliceIn.sum([])
  end

  defmodule Int64SliceOut do
    use Zigler, app: :zigler

    ~Z"""
    @nif("make")
    fn make(length: i64) []i64 {

      // presume that the user has been able to put some slice
      // into memory some other way.

      var binary = e.enif_alloc(@intCast(usize, length) * 8);
      var array = @ptrCast([*]i64, @alignCast(@alignOf(*i64), binary));
      var result = array[0..@intCast(usize, length)];
      var index: i64 = 0;

      while (index < length) {
        result[@intCast(usize, index)] = index;
        index += 1;
      }

      return result;
    }
    """
  end

  test "we can get back an i64 slice type" do
    assert [] == Int64SliceOut.make(0)
    assert [0, 1, 2] == Int64SliceOut.make(3)
    assert [0, 1, 2, 3, 4] == Int64SliceOut.make(5)
  end

  defmodule Float64SliceIn do
    use Zigler, app: :zigler

    ~Z"""
    @nif("sum")
    fn sum(slice: []f64) f64 {
      var total: f64 = 0;
      var index: usize = 0;

      while (index < slice.len) {
        total += slice[index];
        index += 1;
      }

      return total;
    }
    """
  end

  test "we can pass in float64 array types" do
    assert 6.0 == Float64SliceIn.sum([1.0, 2.0, 3.0])
    assert 10.0 == Float64SliceIn.sum([1.0, 2.0, 3.0, 4.0])
    assert 0.0 == Float64SliceIn.sum([])
  end

  defmodule Float64SliceOut do
    use Zigler, app: :zigler

    ~Z"""
    @nif("make")
    fn make(length: i64) []f64 {

      // presume that the user has been able to put some slice
      // into memory some other way.

      var binary = e.enif_alloc(@intCast(usize, length) * 8);
      var array = @ptrCast([*]f64, @alignCast(@alignOf(*f64), binary));
      var result = array[0..@intCast(usize, length)];
      var index: i64 = 0;
      var value: f64 = 0.0;

      while (index < length) {
        result[@intCast(usize, index)] = value;
        index += 1;
        value += 1.0;
      }

      return result;
    }
    """
  end

  test "we can get back an f64 slice type" do
    assert [] == Float64SliceOut.make(0)
    assert [0.0, 1.0, 2.0] == Float64SliceOut.make(3)
    assert [0.0, 1.0, 2.0, 3.0, 4.0] == Float64SliceOut.make(5)
  end

end
