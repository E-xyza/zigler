#guard against this cblas.h not existing.
if File.exists?("/usr/include/x86_64-linux-gnu/cblas.h") do

defmodule ZiglerTest.ZigTest.Blas do
  @moduledoc false

  use Zig,
  libs: ["/usr/lib/x86_64-linux-gnu/blas/libblas.so"],
  system_include_dirs: ["/usr/include/x86_64-linux-gnu"]

  ~Z"""
  const blas = @cImport({
    @cInclude("cblas.h");
  });

  /// nif: blas_axpy/3
  fn blas_axpy(env: beam.env, a: f64, x: []f64, y: []f64) beam.term {

    if (x.len != y.len) {
      return beam.raise_function_clause_error(env);
    }

    blas.cblas_daxpy(@intCast(c_int, x.len), a, x.ptr, 1, y.ptr, 1);

    return beam.make_f64_list(env, y) catch {
      return beam.raise_function_clause_error(env);
    };
  }

  test "blas works in zig" {
    var a: f64 = 2.0;
    var x: []f64 = beam.allocator.alloc(f64, 2) catch unreachable;
    var y: []f64 = beam.allocator.alloc(f64, 2) catch unreachable;

    // set values in x and y
    x[0] = 1.0; x[1] = 2.0;
    y[0] = 3.0; y[1] = 4.0;

    // run the function.
    var beam_res = blas_axpy(beam.test_env, a, x, y);

    // retrieve the result parts.
    var res = beam.get_slice_of(f64, beam.test_env, beam_res) catch unreachable;
    assert(res[0] == 5.0);
    assert(res[1] == 8.0);
  }
  """
end

end
