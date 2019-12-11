#guard against this cblas.h not existing (a situation).
if File.exists?("/usr/include/x86_64-linux-gnu/cblas.h") do

defmodule Zigler.LibraryTest do

  use ExUnit.Case, async: true

  # guard against this library not existing.

  if File.exists?("/usr/lib/x86_64-linux-gnu/blas/libblas.so") do

    defmodule BlasDynamic do
      use Zigler, app: :zigler, libs: ["/usr/lib/x86_64-linux-gnu/blas/libblas.so"]

      ~Z"""
      const blas = @cImport({
        @cInclude("/usr/include/x86_64-linux-gnu/cblas.h");
      });

      /// nif: blas_axpy/3
      fn blas_axpy(env: beam.env, a: f64, x: []f64, y: []f64) beam.term {

        if (x.len != y.len) {
          return beam.throw_function_clause_error(env);
        }

        blas.cblas_daxpy(@intCast(c_int, x.len), a, &x[0], 1, &y[0], 1);

        return beam.make_f64_list(env, y) catch {
          return beam.throw_function_clause_error(env);
        };
      }
      """
    end

    test "we can use dynamically-linked blas" do
      # returns aX+Y
      assert [11.0, 18.0] == BlasDynamic.blas_axpy(3.0, [2.0, 4.0], [5.0, 6.0])
    end

  end

  if File.exists?("/usr/lib/x86_64-linux-gnu/blas/libblas.a") do

    defmodule BlasStatic do
      use Zigler, app: :zigler, libs: ["/usr/lib/x86_64-linux-gnu/blas/libblas.a"]

      ~Z"""
      const blas = @cImport({
        @cInclude("/usr/include/x86_64-linux-gnu/cblas.h");
      });

      /// nif: blas_axpy/3
      fn blas_axpy(env: beam.env, a: f64, x: []f64, y: []f64) beam.term {

        if (x.len != y.len) {
          return beam.throw_function_clause_error(env);
        }

        blas.cblas_daxpy(@intCast(c_int, x.len), a, &x[0], 1, &y[0], 1);

        return beam.make_f64_list(env, y) catch {
          return beam.throw_function_clause_error(env);
        };
      }
      """
    end

    test "we can use statically-linked blas" do
      # returns aX+Y
      assert [11.0, 18.0] == BlasStatic.blas_axpy(3.0, [2.0, 4.0], [5.0, 6.0])
    end
  end

end

end
