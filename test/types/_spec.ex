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
      packed_struct_fn_map_return: [return: :map],
      extern_struct_fn_binary_return: [return: :binary],
      extern_struct_fn_internal_binary_return: [return: {:map, value: :binary}]
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

  pub fn slice_array_u32_fn(data: [][3]u32) [][3]u32 { return data;}

  const RequiredStruct = struct { value: u32 };

  pub fn required_struct_fn(data: RequiredStruct) RequiredStruct { return data; }

  const OptionalStruct = struct { value: u32 = 47 };

  pub fn optional_struct_fn(data: OptionalStruct) OptionalStruct { return data; }

  const PackedStruct = packed struct { value: u32 };

  pub fn packed_struct_fn(data: PackedStruct) PackedStruct { return data; }

  pub fn packed_struct_fn_map_return(data: PackedStruct) PackedStruct { return data; }

  const ExternStruct = extern struct { value: u32 };

  pub fn extern_struct_fn(data: ExternStruct) ExternStruct { return data; }

  pub fn extern_struct_fn_binary_return(data: ExternStruct) ExternStruct { return data; }

  const Extern2Struct = struct { value: [2]u32 };

  pub fn extern_struct_fn_internal_binary_return(data: Extern2Struct) Extern2Struct { return data; }
  """
end
