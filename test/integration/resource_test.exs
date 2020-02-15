defmodule ZiglerTest.Integration.ResourceTest do

  # tests to make sure that the basics of a resource lifecycle works.

  use ExUnit.Case, async: true
  use Zigler

  ~Z"""
  /// resource: test_res definition
  const test_res = i64;

  /// nif: create_resource/1
  fn create_resource(env: beam.env, value: i64) beam.term {
    return beam.resource.create(i64, env, __resource_type__(test_res), value)
      catch beam.raise_function_clause_error(env);
  }

  /// nif: retrieve_resource/1
  fn retrieve_resource(env: beam.env, value: beam.term) i64 {
    return beam.resource.fetch(i64, env, __resource_type__(test_res), value) catch 0;
  }
  """

  describe "using the basic resource form" do
    test "the resource lifecycle is accessible" do
      rsrc = create_resource(47)
      assert 47 == retrieve_resource(rsrc)
    end
  end

  ~Z"""
  /// resource: test_res_alt definition
  const test_res_alt = i64;

  /// nif: create_resource_alt/1
  fn create_resource_alt(env: beam.env, value: i64) beam.term {
    return __resource__.create(test_res, env, value)
      catch beam.raise_function_clause_error(env);
  }

  /// nif: retrieve_resource_alt/1
  fn retrieve_resource_alt(env: beam.env, value: beam.term) i64 {
    return __resource__.fetch(test_res_alt, env, value) catch 0;
  }
  """

  describe "using the special resource form" do
    test "the resource lifecycle is accessible" do
      rsrc = create_resource_alt(47)
      assert 47 == retrieve_resource_alt(rsrc)
    end
  end

  #~Z"""
  #/// resource: test_pid_res definition
  #const test_pid_res = beam.pid;
#
  #/// resource: test_pid_res cleanup
  #fn test_pid_res_cleanup(env: beam.env, pid: *beam.pid) void {
  #  msg = beam.make_atom(env, c"ok");
  #  beam.send(env, pid.*, null, msg);
  #}
#
  #/// nif: create_pid_resource/1
  #fn create_pid_resource(env: beam.env, value: beam.pid) beam.term {
  #  return beam.resource.create(test_pid_res, env, resource_type(test_pid_res), value)
  #    catch beam.raise_function_clause_error(env);
  #}
  #"""
  #describe "erlang resources" do
  #  test "can be cleaned up properly in the basic case" do
  #    test_pid = self()
  #    spawn(fn -> create_pid_resource(test_pid) end)
  #    assert_receive :ok
  #  end
  #end

end
