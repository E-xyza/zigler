defmodule ZiglerTest.ResourceTest do
  use ExUnit.Case

  use Zigler, app: :zigler,
    resources: [:async_test]

  ~Z"""
  /// destructor: async_test
  fn destroy_async_test(env: beam.env, obj: *void) void {
    // nothing needs to happen since this object is a single int64
  }

  /// nif: make_int_resource/1
  fn make_int_resource(env: beam.env, val: i64) beam.term {
    return beam.resource.create(i64, env, async_test, val)
      catch |err| return beam.throw_enomem(env);
  }

  // nif: fetch_int_resource/1
  //fn fetch_int_resource(env: beam.env, resource: beam.res) i64 {
  //  var result = beam.resource.fetch(env, resource);
  //}
  """

  test "a resource can be passed between function calls" do
    resource = make_int_resource(47)
    #assert 47 == fetch_int_resource(resource)
  end
end
