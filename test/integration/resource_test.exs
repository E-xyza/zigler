#defmodule ZiglerTest.Integration.ResourceTest do
#
#  # tests to make sure that the basics of a resource lifecycle works.
#
#  use ExUnit.Case, async: true
#  use Zigler
#
#  ~Z"""
#  /// resource: test_res definition
#  const test_res = i64;
#
#  /// nif: create_resource/1
#  fn create_resource(env: beam.env, value: i64) beam.term {
#    return beam.resource.create(i64, env, __resource_type__(test_res), value)
#      catch beam.raise_function_clause_error(env);
#  }
#
#  /// nif: update_resource/2
#  fn update_resource(env: beam.env, resource: beam.term, new_value: i64) beam.term {
#    beam.resource.update(i64, env, __resource_type__(test_res), resource, new_value)
#      catch return beam.raise_function_clause_error(env);
#    return beam.make_atom(env, "ok");
#  }
#
#  /// nif: retrieve_resource/1
#  fn retrieve_resource(env: beam.env, value: beam.term) i64 {
#    return beam.resource.fetch(i64, env, __resource_type__(test_res), value) catch 0;
#  }
#  """
#
#  describe "using the basic resource form" do
#    test "the resource lifecycle is accessible" do
#      rsrc = create_resource(47)
#      assert 47 == retrieve_resource(rsrc)
#    end
#
#    test "and resources can be updated" do
#      rsrc = create_resource(42)
#      assert 42 == retrieve_resource(rsrc)
#      assert :ok == update_resource(rsrc, 47)
#      assert 47 == retrieve_resource(rsrc)
#    end
#  end
#
#  ~Z"""
#  /// resource: test_res_alt definition
#  const test_res_alt = i64;
#
#  /// nif: create_resource_alt/1
#  fn create_resource_alt(env: beam.env, value: i64) beam.term {
#    return __resource__.create(test_res, env, value)
#      catch beam.raise_function_clause_error(env);
#  }
#
#  /// nif: update_resource_alt/2
#  fn update_resource_alt(env: beam.env, resource: beam.term, new_value: i64) beam.term {
#    __resource__.update(test_res, env, resource, new_value)
#      catch return beam.raise_function_clause_error(env);
#    return beam.make_atom(env, "ok");
#  }
#
#  /// nif: retrieve_resource_alt/1
#  fn retrieve_resource_alt(env: beam.env, value: beam.term) i64 {
#    return __resource__.fetch(test_res_alt, env, value) catch 0;
#  }
#  """
#
#  describe "using the special __resource__ form" do
#    test "the resource lifecycle is accessible" do
#      rsrc = create_resource_alt(47)
#      assert 47 == retrieve_resource_alt(rsrc)
#    end
#
#    test "and resources can be updated" do
#      rsrc = create_resource_alt(42)
#      assert 42 == retrieve_resource_alt(rsrc)
#      assert :ok == update_resource_alt(rsrc, 47)
#      assert 47 == retrieve_resource_alt(rsrc)
#    end
#  end
#
#  ~Z"""
#  /// resource: test_pid_res definition
#  const test_pid_res = beam.pid;
#
#  /// instrument the custom cleanup handler so that instead of
#  /// cleaning anything up (we don't need to since it's a raw pid)
#  /// we send the pid a message.  That way we can track when
#  /// cleanup events have happened.
#  ///
#  /// resource: test_pid_res cleanup
#  fn test_pid_res_cleanup(env: beam.env, pid: *test_pid_res) void {
#    var msg = beam.make_atom(env, "done");
#    var _res = beam.send(env, pid.*, null, msg);
#  }
#
#  /// nif: create_pid_resource/1
#  fn create_pid_resource(env: beam.env, value: beam.pid) beam.term {
#    var res = __resource__.create(test_pid_res, env, value)
#      catch return beam.raise_function_clause_error(env);
#    // release it so that it can be garbage collected.
#    __resource__.release(test_pid_res, env, res);
#    return res;
#  }
#  """
#
#  describe "erlang resources" do
#    test "can be cleaned up properly in the basic case" do
#      test_pid = self()
#      pid = spawn(fn -> create_pid_resource(test_pid) end)
#      Process.sleep(50)
#      refute Process.alive?(pid)
#      assert_receive :done
#    end
#
#    defp wait_200_millis(test_pid) do
#      # holds on to this object for 200 milliseconds
#      Process.sleep(200)
#    end
#
#    test "can be cleaned up properly in the more complex case" do
#      test_pid = self()
#      pid = spawn(fn ->
#        res = create_pid_resource(test_pid)
#        spawn(fn -> wait_200_millis(res) end)
#      end)
#      # wait 100 ms
#      Process.sleep(100)
#      # hasn't been destroyed yet.
#      refute_received :done
#      # wait another 100 ms to be sure that the spawned threads are ok.
#      Process.sleep(150)
#      # now the reference counters should be exhausted.
#      assert_receive :done
#    end
#  end
#
#end
#
