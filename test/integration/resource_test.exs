defmodule ZiglerTest.Integration.ResourceTest do

  # tests to make sure that the basics of a resource lifecycle works.

  use ExUnit.Case, async: true
  use Zigler

  ~Z"""
  /// resource: test_res
  const test_res = i64;

  /// nif: create_resource/1
  fn create_resource(env: beam.env, value: i64) beam.term {
    return beam.resource.create(i64, env, resource_type(test_res), value)
      catch beam.raise_function_clause_error(env);
  }

  /// nif: retrieve_resource/1
  fn retrieve_resource(env: beam.env, value: beam.term) i64 {
    return beam.get_i64(env, value) catch 0;
  }
  """

  test "the resource lifecycle" do
    rsrc = create_resource(47)
    |> IO.inspect(label: "22")
    assert 47 == retrieve_resource(rsrc)
  end

end
