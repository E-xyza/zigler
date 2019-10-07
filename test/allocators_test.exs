defmodule ZigTest.AllocatorsTest do
  use ExUnit.Case

  defmodule BasicAllocator do
    use Zigler, app: :zigler

    ~Z"""
    @nif("alloctest")
    fn alloctest(env: ?*e.ErlNifEnv, length: i64) e.ErlNifTerm {
      const enomem = "enomem";
      const enomem_atom = e.enif_make_atom_len(env, @ptrCast([*c]const u8, &enomem[0]), 6);

      var usize_length = @intCast(usize, length);

      var slice = elixir.allocator.alloc(u8, usize_length)
        catch | _err | return e.enif_raise_exception(env, enomem_atom);
      defer elixir.allocator.free(slice);

      // fill the slice with letters
      for (slice) | _char, i | {
        slice[i] = 97 + @intCast(u8, i);
      }

      return e.enif_make_atom_len(env, slice.ptr, slice.len);
    }

    @nif("realloctest")
    fn realloctest(env: ?*e.ErlNifEnv, length: i64) e.ErlNifTerm {
      const enomem = "enomem";
      const enomem_atom = e.enif_make_atom_len(env, @ptrCast([*c]const u8, &enomem[0]), 6);

      var usize_length = @intCast(usize, length);

      var slice = elixir.allocator.alloc(u8, usize_length)
        catch | _err | return e.enif_raise_exception(env, enomem_atom);
      defer elixir.allocator.free(slice);

      var slice2 = elixir.allocator.realloc(slice, usize_length * 2)
        catch | _err | return e.enif_raise_exception(env, enomem_atom);

      // fill the slice with letters
      for (slice2) | _char, i | {
        slice2[i] = 97 + @intCast(u8, i);
      }

      return e.enif_make_atom_len(env, slice2.ptr, slice2.len);
    }
    """
  end

  test "elixir basic allocator works" do
    assert :ab == BasicAllocator.alloctest(2)
    assert :abc == BasicAllocator.alloctest(3)

    assert :abcd == BasicAllocator.realloctest(2)
    assert :abcdef == BasicAllocator.realloctest(3)
  end

  defmodule PersistentMemory do
    use Zigler, app: :zigler

    # proves that you can do something crazy, like keep memory around.
    # don't do this in real code.

    ~Z"""
    var global_nothing = [_]u8{};
    var global_slice = global_nothing[0..0];

    @nif("allocate")
    fn allocate(env: ?*e.ErlNifEnv, length: i64) bool {
      const enomem = "enomem";
      const enomem_atom = e.enif_make_atom_len(env, @ptrCast([*c]const u8, &enomem[0]), 6);

      var usize_length = @intCast(usize, length);

      global_slice = elixir.allocator.alloc(u8, usize_length) catch | _err | {
        var execption = e.enif_raise_exception(env, enomem_atom);
        return false;
      };
      // don't defer a free here (don't do this in real life!!!)

      // fill the slice with letters
      for (global_slice) | _char, i | {
        global_slice[i] = 97 + @intCast(u8, i);
      }

      return true;
    }

    @nif("fetch")
    fn fetch(env: ?*e.ErlNifEnv, dummy: i64) e.ErlNifTerm {
      return e.enif_make_atom_len(env, global_slice.ptr, global_slice.len);
    }
    """
  end

  test "elixir persistent memory works" do
    assert true == PersistentMemory.allocate(2);
    assert :ab == PersistentMemory.fetch(0);
  end
end
