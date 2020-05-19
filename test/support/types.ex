defmodule ZiglerTest.Types do

  @moduledoc false

  use Zigler, dry_run: true

  @spec dummy_integer(integer) :: integer
  def dummy_integer(a), do: a

  ~Z"""
  /// nif: void_out/0
  fn void_out() void {}

  /////////////////////////////////////////////////////////////////////////////
  // SCALARS

  /// nif: u32_in_out/1
  fn u32_in_out(x: u32) u32 { return x; }

  /// nif: i32_in_out/1
  fn i32_in_out(x: i32) i32 { return x; }

  /// nif: i64_in_out/1
  fn i64_in_out(x: i64) i64 { return x; }

  /// nif: f64_in_out/1
  fn f64_in_out(x: f64) f64 { return x; }

  /// nif: bool_in_out/1
  fn bool_in_out(x: bool) bool { return x; }

  /////////////////////////////////////////////////////////////////////////////
  // BEAM

  /// nif: term_in_out/1
  fn term_in_out(x: beam.term) beam.term { return x; }

  /// nif: eterm_in_out/1
  fn eterm_in_out(x: e.ErlNifTerm) e.ErlNifTerm { return x; }

  /// nif: pid_in_out/1
  fn pid_in_out(x: beam.pid) beam.pid { return x; }

  /// nif: epid_in_out/1
  fn epid_in_out(x: e.ErlNifPid) e.ErlNifPid { return x; }

  /// nif: atom_in_out/1
  fn atom_in_out(x: beam.atom) beam.atom { return x; }

  /////////////////////////////////////////////////////////////////////////////
  // SLICES

  /// nif: str_in_out/1
  fn str_in_out(x: []u8) []u8 { return x; }

  /// nif: islice_in_out/1
  fn islice_in_out(x: []i64) []i64 { return x; }

  /////////////////////////////////////////////////////////////////////////////
  // MULTIARGUMENT

  /// nif: multiarg/2
  fn multiarg(a: i64, b: f64) f64 { return a * b; }

  /// nif: env_zero/0
  fn env_zero(env: beam.env) i64 { return 47; }

  /// nif: eenv_zero/0
  fn eenv_zero(env: ?*e.ErlNifEnv) i64 { return 47; }

  /// nif: env_one/1
  fn env_one(env: beam.env, x: i64) i64 { return x; }

  /// nif: eenv_one/1
  fn eenv_one(env: ?*e.ErlNifEnv, x: i64) i64 { return x; }
  """
end
