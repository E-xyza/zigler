# CXX integration

Zigler offers several tools to integrate your code with C and C++ code.

## compiling C using the C toolchain

If you want to compile C or C++ files using the C and C++ toolchain bundled
with the zig programming language, you should include a path to your
include directory and a source path or list of source paths to be compiled,
via the `include_dir` and `src` module options.

> ### source paths {: .info }
>
> source paths may contain a trailing *, which will pull all c or c++ files
> in that directory, (but not subdirectory)

### included.h
```c
// forwarded function definition
int plus_one(int);
```

### src/src.c
```c
int plus_one(int value) {
    return value + 1;
}
```

### Elixir code
```elixir
defmodule CompilingC do
  use ExUnit.Case, async: true
  use Zig, 
    otp_app: :zigler,
    c: [include_dirs: "include", src: "src/*"] 

  ~Z"""
  const c = @cImport(@cInclude("included.h"));

  pub const plus_one = c.plus_one;
  """

  test "c plus one" do
    assert 48 = plus_one(47)
  end
end
```

## linking against a C abi library

This example shows you how to link in a system library (which can be `.a`,
`.so`, `.obj`, or `.dll`).  Zig will resolve the extension based on the
operating system native rules.  To use the functions in the library, there
must also be an associated `.h` file with `extern` functions.

In this example we'll use the `cblas_dasum` function, which takes a length,
an pointer to double-precision floating point list, and a integer stride.
The result is a sum of the numbers in the list.

The [rules for collections](#2-collections.html) apply to functions that
are directly imported from C files.

```elixir
if {:unix, :linux} == :os.type() do
# currently we only have access to the BLAS library on linux CI actions, so
# it's unavailable for other operating systems for automated testing purposes

  defmodule LibraryTest do
    use ExUnit.Case, async: true
    use Zig, 
      otp_app: :zigler,
      link_lib: {:system, "blas"}

    ~Z"""
    pub const dasum = @cImport(@cInclude("cblas.h")).cblas_dasum;
    """

    test "dasum" do
      assert 6.0 == dasum(3, [1.0, 2.0, 3.0], 1)
    end
  end
```

> ### linking against libcpp: {: .info }
>
> if you need to link against `libcpp`, the library has a special-cased option: 
> `link_libcpp: true`

## Easy-C

It's also possible to also automatically create nifs without writing zig 
function shims.  This works either with linking an external library with 
`link_lib` or building your own code with `src`.

Because the C ABI exposes all functions publically in a global namespace, 
we can't use zigler's automatic detection to decide which functions to
surface.

Here are the steps to using `easy_c`:

- declare which header file you'd like to use with the `easy_c` module option
  - you may want to add `include_dir` if the header isn't a system C header.
- add a `link_lib` or `src` option to make sure that the functions are built.
- declare which functions you'd like to hoist into the module.

In this example we'll use the `cblas_daxpy` function, which takes a length,
a double-precision `a` value, a list (`x`) of double-precision values, a stride
for the `x` values, a list (`y`) of double-precision values, and a stride for 
`y`. 

It then calculates the linear transformation `ax + y` using these vectors.

> ### "in-out" parameters {: .info }
>
> The `y` parameter in this function is an "in-out" parameter.  Instead of 
> returning a result, the results are written into the y pointer.
>
> to specify that it's an in-out parameter, we put an integer (which is the
> 0-indexed index of the argument which is the out parameter).
>
> since the returned pointer doesn't have a specified value, we have to 
> specify a `length` option, which could either be a fixed number, or
> `{:arg, n}` where `n` is the argument index corresponding to a variable
> length.
>
> We can also use [nif options like alias](4-nif_options.html#alias)
> and [the return option `binary`](2-collections.html#selecting-output-type)
> alongside these special `easy_c` options.

```elixir
  defmodule EasyCTest do
    use ExUnit.Case, async: true
    use Zig, 
      otp_app: :zigler,
      easy_c: "cblas.h",
      link_lib: {:system, "blas"},
      nifs: [
        cblas_daxpy: [return: [4, length: {:arg, 0}]],
        cblas_daxpy_bin: [alias: :cblas_daxpy, return: [4, :binary, length: {:arg, 0}]]
      ]

    test "daxpy as a list" do
      assert [7.0, 11.0, 15.0] == cblas_daxpy(3, 3.0, [1.0, 2.0, 3.0], 1, [4.0, 5.0, 6.0], 1)
    end

    test "daxpy as a binary" do
      assert <<7.0::float-native, 11.0::float-native, 15.0::float-native>> ==
               cblas_daxpy_bin(3, 3.0, [1.0, 2.0, 3.0], 1, [4.0, 5.0, 6.0], 1)
    end
  end
end
# module
```