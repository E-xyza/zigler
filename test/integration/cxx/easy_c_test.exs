# don't test this unless the system has the cblas libray
# for now we can only guarantee that for linux.
if {:unix, :linux} == :os.type() do
  defmodule ZiglerTest.Integration.CXX.EasyCTest do
    use ZiglerTest.IntegrationCase, async: true

    use Zig,
      otp_app: :zigler,
      easy_c: "cblas.h",
      link_lib: {:system, "blas"},
      # leak_check: true,
      nifs: [
        :cblas_dasum,
        cblas_daxpy: [return: [4, length: {:arg, 0}]],
        daxpy_bin: [alias: :cblas_daxpy, return: [4, :binary, length: {:arg, 0}]]
      ]

    describe "dasum" do
      # the dasum function takes an array of doubles and adds them up together.
      # first argument is count, second argument is the array, third argument is the stride.
      test "works" do
        assert 6.0 == cblas_dasum(3, [1.0, 2.0, 3.0], 1)
      end

      test "works with float binaries" do
        assert 6.0 ==
                 cblas_dasum(3, <<1.0::float-native, 2.0::float-native, 3.0::float-native>>, 1)
      end

      test "will reject invalid input" do
        assert_raise ArgumentError,
                     "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: integer (for `c_int`)\n     got: `:foo`\n",
                     fn ->
                       cblas_dasum(:foo, [1.0, 2.0, 3.0], 1)
                     end
      end
    end

    describe "daxpy" do
      # the daxpy function performs a linear combination of two arrays:  aX + Y.  Result is stored
      # in the Y array
      #
      # arguments:
      # - count
      # - a value
      # - X array
      # - X stride
      # - Y array *mutable
      # - Y stride
      test "works" do
        assert [7.0, 11.0, 15.0] == cblas_daxpy(3, 3.0, [1.0, 2.0, 3.0], 1, [4.0, 5.0, 6.0], 1)
      end

      test "can be aliased to output binaries" do
        assert <<7.0::float-native, 11.0::float-native, 15.0::float-native>> ==
                 daxpy_bin(3, 3.0, [1.0, 2.0, 3.0], 1, [4.0, 5.0, 6.0], 1)
      end
    end
  end
end
