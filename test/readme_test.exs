defmodule ZiglerTest.ReadmeTest do
  # validates all of the claims in the readme.
  use ExUnit.Case

  defmodule ExampleZig do
    use Zigler, app: :zigler
    ~Z"""
    @nif("example_fun")
    fn example_fun(value1: f64, value2: f64) bool {
      return value1 > value2;
    }
    """
  end

  test "example_zig" do
    refute ExampleZig.example_fun(0.1, 0.4)
    assert ExampleZig.example_fun(0.8, -0.8)
  end

  defmodule ZigCollections do
    use Zigler, app: :zigler
    ~Z"""
    @nif("string_count")
    fn string_count(string: []u8) i64 {
      return @intCast(i64, string.len);
    }

    @nif("list_sum")
    fn list_sum(array: []f64) f64 {
      var sum: f64 = 0.0;
      for(array) | item | {
        sum += item;
      }
      return sum;
    }
    """
  end

  test "zig_collections" do
    assert 9 == ZigCollections.string_count("hello zig")
    assert 6.0 == ZigCollections.list_sum([1.0, 2.0, 3.0])
  end

  defmodule Allocations do
    use Zigler, app: :zigler
    ~Z"""
    @nif("double_atom")
    fn double_atom(env: elixir.env, string: []u8) elixir.atom {

      var double_string = elixir.allocator.alloc(u8, string.len * 2)
        catch elixir.enomem(env);

      defer elixir.allocator.free(double_string);

      for (string) | char, i | {
        double_string[i] = char;
        double_string[i + string.len] = char;
      }

      return elixir.make_atom(env, double_string);
    }
    """
  end

  test "allocations" do
    assert :foofoo == Allocations.double_atom("foo")
  end
end
