defmodule ZiglerTest.Integration.BeamTypeTest do
  use ExUnit.Case, async: true
  use Zig

  @moduletag :integration

  ~Z"""
  /// nif: pass_beam_term/1
  fn pass_beam_term(val: beam.term) beam.term {
    return val;
  }
  """

  test "generic beam_term" do
    assert 1 == pass_beam_term(1)
    assert :foo == pass_beam_term(:foo)
    assert "bar" == pass_beam_term("bar")
    assert self() == pass_beam_term(self())
  end

  ~Z"""
  /// nif: pass_e_erl_nif_term/1
  fn pass_e_erl_nif_term(val: e.ErlNifTerm) e.ErlNifTerm {
    return val;
  }
  """

  test "generic e_erl_nif_term" do
    assert 1 == pass_e_erl_nif_term(1)
    assert :foo == pass_e_erl_nif_term(:foo)
    assert "bar" == pass_e_erl_nif_term("bar")
    assert self() == pass_e_erl_nif_term(self())
  end

  # NB: there is no beam.make_pid!!
  ~Z"""
  /// nif: pass_beam_pid/1
  fn pass_beam_pid(env: beam.env, val: beam.pid) void {
    _ = beam.send(env, val, beam.make_i64(env, 47));
  }
  """

  test "generic pid" do
    pass_beam_pid(self())
    assert_receive 47

    assert_raise FunctionClauseError, fn ->
      pass_beam_pid(:foo)
    end

    assert_raise FunctionClauseError, fn ->
      pass_beam_pid(47)
    end
  end

  ~Z"""
  /// nif: pass_erl_nif_pid/1
  fn pass_erl_nif_pid(env: beam.env, val: e.ErlNifPid) void {
    _ = beam.send(env, val, beam.make_i64(env, 47));
  }
  """

  test "generic erl_nif_pid" do
    pass_erl_nif_pid(self())
    assert_receive 47

    assert_raise FunctionClauseError, fn ->
      pass_erl_nif_pid(:foo)
    end

    assert_raise FunctionClauseError, fn ->
      pass_erl_nif_pid(47)
    end
  end
end
