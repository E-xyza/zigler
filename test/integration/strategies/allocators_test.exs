defmodule ZiglerTest.Integration.Strategies.AllocatorsTest do
  use ExUnit.Case, async: true

  use Zigler

  @moduletag :allocators

  ~Z"""
  /// nif: alloctest/1
  fn alloctest(env: ?*e.ErlNifEnv, length: i64) e.ErlNifTerm {
    var usize_length = @intCast(usize, length);
    var slice = beam.allocator.alloc(u8, usize_length) catch {
      return beam.raise_enomem(env);
    };
    defer beam.allocator.free(slice);
    // fill the slice with letters
    for (slice) | _char, i | {
      slice[i] = 97 + @intCast(u8, i);
    }
    return e.enif_make_atom_len(env, slice.ptr, slice.len);
  }

  /// nif: realloctest/1
  fn realloctest(env: ?*e.ErlNifEnv, length: i64) e.ErlNifTerm {
    var usize_length = @intCast(usize, length);
    var slice = beam.allocator.alloc(u8, usize_length) catch {
      return beam.raise_enomem(env);
    };

    var slice2 = beam.allocator.realloc(slice, usize_length * 2) catch {
      return beam.raise_enomem(env);
    };

    defer beam.allocator.free(slice2);

    // fill the slice with letters
    for (slice2) | _char, i | {
      slice2[i] = 97 + @intCast(u8, i);
    }
    return e.enif_make_atom_len(env, slice2.ptr, slice2.len);
  }
  """

  test "elixir basic allocator works" do
    assert :ab == alloctest(2)
    assert :abc == alloctest(3)

    assert :abcd == realloctest(2)
    assert :abcdef == realloctest(3)
  end

  # proves that you can do something crazy, like keep memory around in global
  # var state.  don't do this in real code.  There are probably better ways of
  # safely doing this with a zig nif (for example, resources).

  ~Z"""
  var global_slice : []u8 = undefined;

  /// nif: allocate/1
  fn allocate(env: ?*e.ErlNifEnv, length: i64) bool {

    var usize_length = @intCast(usize, length);

    global_slice = beam.allocator.alloc(u8, usize_length) catch {
      // don't do this in real life!
      unreachable;
    };
    // NB: don't defer a free here (don't do this in real life!!!)

    // fill the slice with letters
    for (global_slice) | _char, i | {
      global_slice[i] = 97 + @intCast(u8, i);
    }

    return true;
  }

  /// nif: fetch/0
  fn fetch(env: ?*e.ErlNifEnv) e.ErlNifTerm {
    return e.enif_make_atom_len(env, global_slice.ptr, global_slice.len);
  }
  """

  test "elixir persistent memory works" do
    assert true == allocate(2)
    assert :ab == fetch()
  end

  ~Z"""
  const at_least = std.mem.Allocator.Exact.at_least;

  fn dealloc(ptr: []u8) u64 {
    beam.large_allocator.free(ptr);
    return @ptrToInt(ptr.ptr);
  }

  /// nif: large_allocator_stress_8/0
  fn large_allocator_stress_8() u64 {
    var result = beam.large_allocator.allocAdvanced(u8, 8, 100, at_least) catch unreachable;
    return dealloc(result);
  }

  /// nif: large_allocator_stress_16/0
  fn large_allocator_stress_16() u64 {
    var result = beam.large_allocator.allocAdvanced(u8, 16, 100, at_least) catch unreachable;
    return dealloc(result);
  }

  /// nif: large_allocator_stress_32/0
  fn large_allocator_stress_32() u64 {
    var result = beam.large_allocator.allocAdvanced(u8, 32, 100, at_least) catch unreachable;
    return dealloc(result);
  }

  /// nif: large_allocator_realloc/0
  fn large_allocator_realloc() u64 {
    var result = beam.large_allocator.allocAdvanced(u8, 32, 100, at_least) catch unreachable;
    _ = beam.large_allocator.realloc(result, 50) catch unreachable;
    return dealloc(result);
  }
  """

  test "allocator stress test" do
    for _ <- 1..200 do
      addr = large_allocator_stress_8()
      assert rem(addr, 8) == 0
    end

    for _ <- 1..200 do
      addr = large_allocator_stress_16()
      assert rem(addr, 16) == 0
    end

    for _ <- 1..200 do
      addr = large_allocator_stress_32()
      assert rem(addr, 32) == 0
    end

    for _ <- 1..200 do
      large_allocator_realloc()
    end
  end

  ~Z"""

  /// nif: gp_alloctest/1
  fn gp_alloctest(env: ?*e.ErlNifEnv, length: usize) beam.term {
    var usize_length = @intCast(usize, length);
    var slice = beam.general_purpose_allocator.allocAdvanced(u8, 4096, usize_length, .exact) catch {
      return beam.raise_enomem(env);
    };
    defer beam.general_purpose_allocator.free(slice);

    return beam.make_u64(env, @ptrToInt(slice.ptr));
  }

  /// nif: gp_realloctest/1
  fn gp_realloctest(env: ?*e.ErlNifEnv, length: usize) beam.term {
    var usize_length = @intCast(usize, length);

    var slice = beam.general_purpose_allocator.allocAdvanced(u8, 4096, usize_length, .exact) catch {
      return beam.raise_enomem(env);
    };

    var slice2 = beam.general_purpose_allocator.realloc(slice, usize_length * 2) catch {
      return beam.raise_enomem(env);
    };

    defer beam.general_purpose_allocator.free(slice2);

    return beam.make_u64(env, @ptrToInt(slice.ptr));
  }

  """

  @tag :gp_allocator
  test "elixir general purpose allocator works" do
    assert rem(gp_alloctest(4096), 4096) == 0
    assert rem(gp_alloctest(10_000), 4096) == 0

    assert rem(gp_realloctest(4096), 4096) == 0
    assert rem(gp_realloctest(10_000), 4096) == 0

    for _ <- 1..200 do
      gp_alloctest(Enum.random(4096..102_400))
    end

    for _ <- 1..200 do
      gp_realloctest(Enum.random(4096..102_400))
    end
  end
end
