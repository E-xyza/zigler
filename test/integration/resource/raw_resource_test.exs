defmodule ZiglerTest.Resource.RawResourceTest do
  use ExUnit.Case, async: true

  use Zig, otp_app: :zigler, resources: [:FooStruct]

  ~Z"""
  const beam = @import("beam");
  const Resource = beam.Resource;
  pub const resource = beam.resources(@import("root"));

  pub const FooStruct = struct {
      payload: u64,
  };

  pub fn new(res: FooStruct) Resource(FooStruct) {
      return resource.create(res, .{}) catch unreachable;
  }

  //pub fn increment(res: *Resource(FooStruct)) void {
  //  const to_increment = resource.unpack(res);
  //  return resource.update(res, .{.payload = to_increment.payload + 1});
  //}

  //pub fn inspect(res: Resource(FooStruct)) u64 {
  //  return resource.unpack(res).payload;
  //}
  """

   test "raw resource function is identified" do
    res = new(%{payload: 47})
    #assert is_reference(res)
    #assert 47 = inspect(res)
    ## increment(res)
    #assert 48 = inspect(res)
   end
end
