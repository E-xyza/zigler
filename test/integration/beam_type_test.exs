defmodule ZiglerTest.Integration.BeamTypeTest do
  use ExUnit.Case, async: true
  use Zigler

  #beam.atom beam.pid

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

end
