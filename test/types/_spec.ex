defmodule ZiglerTest.Types.Spec do
  @moduledoc false
  @compile :debug_info

  use Zig,
    otp_app: :zigler,
    nifs: [
      ...,
      enum_fn_integer_return: [return: :integer],
      u32_array_fn_binary_return: [return: :binary],
      u8_array_fn_list_return: [return: :list],
      array_of_arrays_fn_list_of_binary_return: [return: [as: {:list, :binary}]],
      array_of_arrays_fn_binary_return: [return: :binary],
      manypointer_list_return_fn: [return: :list],
      slice_f64_fn_binary_return: [return: :binary],
      slice_u8_fn_list_return: [return: :list],
    ]

  ~Z"""
  const beam = @import("beam");

  pub fn bool_fn(data: bool) bool { return data; }

  const E = enum{ ok, @"error" };
  pub fn enum_fn(data: E) E { return data; }

  pub fn enum_fn_integer_return(data: E) E { return data; }

  pub fn float_fn(data: f64) f64 { return data; }

  pub fn u5_fn(data: u5) u5 { return data; }

  pub fn u32_fn(data: u32) u32 { return data; }

  pub fn i32_fn(data: i32) i32 { return data; }

  pub fn u128_fn(data: u128) u128 { return data; }

  pub fn optional_fn(data: ?u32) ?u32 { return data; }

  pub fn pid_fn(pid: beam.pid) beam.pid { return pid; }

  pub fn term_fn(data: beam.term) beam.term { return data; }

  pub fn void_fn() void { }

  pub fn u32_array_fn(data: [4]u32) [4]u32 { return data;}

  pub fn u32_array_fn_binary_return(data: [4]u32) [4]u32 { return data;}

  pub fn u8_array_fn(data: [4]u8) [4]u8 { return data; }

  pub fn u8_array_fn_list_return(data: [4]u8) [4]u8 { return data; }

  pub fn array_of_arrays_fn(data: [4][4]u32) [4][4]u32 { return data;}

  pub fn array_of_arrays_fn_list_of_binary_return(data: [4][4]u32) [4][4]u32 { return data;}

  pub fn array_of_arrays_fn_binary_return(data: [4][4]u32) [4][4]u32 { return data;}

  pub fn sentinel_terminated_array_fn(data: [4:0]u8) [4:0]u8 { return data;}

  // pub fn cpointer_fn(data: [*c:0]u8) [*c:0]

  pub fn manypointer_u8_fn(data: [*]u8) u8 { return data[0]; }

  pub fn manypointer_u32_fn(data: [*]u32) u32 { return data[0]; }

  pub fn manypointer_return_fn() [*:0]const u8 { return @ptrCast(&"hello"); }

  pub fn manypointer_list_return_fn() [*:0]const u8 { return @ptrCast(&"hello"); }

  pub fn slice_f64_fn(data: []f64) []f64 { return data; }

  pub fn slice_f64_fn_binary_return(data: []f64) []f64 { return data; }

  pub fn slice_u8_fn(data: []u8) []u8 { return data; }

  pub fn slice_u8_fn_list_return(data: []u8) []u8 { return data; }
  """
end
