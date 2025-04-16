defmodule ZiglerTest.OptionsTest do
  use ExUnit.Case, async: true

  defp make_module(opts), do: Zig.Module.new(opts, __ENV__)

  describe "for the module_code_path option" do
    test "string is ok" do
      assert %Zig.Module{} = make_module(otp_app: :zigler, module_code_path: "foobar")
    end

    test "iodata is ok" do
      assert %Zig.Module{} = make_module(otp_app: :zigler, module_code_path: ["foo" | "bar"])
    end

    test "non-path raises" do
      assert_raise CompileError, "test/options_test.exs:4: module_code_path must be a path", fn ->
        make_module(otp_app: :zigler, module_code_path: :foo)
      end
    end
  end

  describe "for the zig_code_path option" do
    test "string is ok" do
      assert %Zig.Module{} = make_module(otp_app: :zigler, zig_code_path: "foobar")
    end

    test "iodata is ok" do
      assert %Zig.Module{} = make_module(otp_app: :zigler, zig_code_path: ["foo" | "bar"])
    end

    test "non-path raises" do
      assert_raise CompileError, "test/options_test.exs:4: zig_code_path must be a path", fn ->
        make_module(otp_app: :zigler, zig_code_path: :foo)
      end
    end
  end

  describe "for c option" do
    test "invalid subkey" do
      assert_raise CompileError,
                   "test/options_test.exs:4: `c` option had invalid key `foo`",
                   fn ->
                     make_module(otp_app: :zigler, c: [foo: :bar])
                   end
    end

    test "include_dirs accepts various forms" do
      local_dir = Path.join(__DIR__, "my_dir")

      assert %{c: %{include_dirs: [^local_dir]}} =
               make_module(otp_app: :zigler, c: [include_dirs: "my_dir"])

      priv_dir = Path.join(:code.priv_dir(:zigler), "my_dir")

      assert %{c: %{include_dirs: [^priv_dir]}} =
               make_module(otp_app: :zigler, c: [include_dirs: {:priv, "my_dir"}])

      assert %{c: %{include_dirs: [system: "system"]}} =
               make_module(otp_app: :zigler, c: [include_dirs: {:system, "system"}])

      assert %{c: %{include_dirs: [^local_dir, ^priv_dir, system: "system"]}} =
               make_module(
                 otp_app: :zigler,
                 c: [include_dirs: ["my_dir", priv: "my_dir", system: "system"]]
               )
    end

    @valid_type_phrase "a string, `{:priv, string}`, `{:system, string}`, or a list of those"

    test "include_dirs not a string list" do
      assert_raise CompileError,
                   "test/options_test.exs:4: `c` option `include_dirs` must be #{@valid_type_phrase}, got: `:moop`",
                   fn ->
                     make_module(otp_app: :zigler, c: [include_dirs: :moop])
                   end
    end

    test "library_dirs accepts various forms" do
      local_dir = Path.join(__DIR__, "my_dir")

      assert %{c: %{library_dirs: [^local_dir]}} =
               make_module(otp_app: :zigler, c: [library_dirs: "my_dir"])

      priv_dir = Path.join(:code.priv_dir(:zigler), "my_dir")

      assert %{c: %{library_dirs: [^priv_dir]}} =
               make_module(otp_app: :zigler, c: [library_dirs: {:priv, "my_dir"}])

      assert %{c: %{library_dirs: [system: "system"]}} =
               make_module(otp_app: :zigler, c: [library_dirs: {:system, "system"}])

      assert %{c: %{library_dirs: [^local_dir, ^priv_dir, system: "system"]}} =
               make_module(
                 otp_app: :zigler,
                 c: [library_dirs: ["my_dir", priv: "my_dir", system: "system"]]
               )
    end

    test "library_dirs not a string list" do
      assert_raise CompileError,
                   "test/options_test.exs:4: `c` option `library_dirs` must be #{@valid_type_phrase}, got: `:moop`",
                   fn ->
                     make_module(otp_app: :zigler, c: [library_dirs: :moop])
                   end
    end

    test "link_lib accepts various forms" do
      local_dir = Path.join(__DIR__, "my_dir")

      assert %{c: %{link_lib: [^local_dir]}} =
               make_module(otp_app: :zigler, c: [link_lib: "my_dir"])

      priv_dir = Path.join(:code.priv_dir(:zigler), "my_dir")

      assert %{c: %{link_lib: [^priv_dir]}} =
               make_module(otp_app: :zigler, c: [link_lib: {:priv, "my_dir"}])

      assert %{c: %{link_lib: [system: "system"]}} =
               make_module(otp_app: :zigler, c: [link_lib: {:system, "system"}])

      assert %{c: %{link_lib: [^local_dir, ^priv_dir, system: "system"]}} =
               make_module(
                 otp_app: :zigler,
                 c: [link_lib: ["my_dir", priv: "my_dir", system: "system"]]
               )
    end

    test "link_lib not a string list" do
      assert_raise CompileError,
                   "test/options_test.exs:4: `c` option `link_lib` must be #{@valid_type_phrase}, got: `:moop`",
                   fn ->
                     make_module(otp_app: :zigler, c: [link_lib: :moop])
                   end
    end

    test "link_libcpp not a boolean" do
      assert_raise CompileError,
                   "test/options_test.exs:4: `c` option `link_libcpp` must be a boolean, got: `:moop`",
                   fn ->
                     make_module(otp_app: :zigler, c: [link_libcpp: :moop])
                   end
    end

    test "src accepts bosic string forms" do
      local_dir = Path.join(__DIR__, "my_dir")

      assert %{c: %{src: [{^local_dir, []}]}} =
               make_module(otp_app: :zigler, c: [src: "my_dir"])
    end

    test "src not a string list" do
      assert_raise CompileError,
                   "test/options_test.exs:4: `c` option `src` must be #{@valid_type_phrase}, got: `:moop`",
                   fn ->
                     make_module(otp_app: :zigler, c: [src: :moop])
                   end
    end
  end

  test "an invalid key" do
    assert_raise CompileError,
                 "test/options_test.exs:4: `foo` is not a valid option",
                 fn ->
                   make_module(otp_app: :zigler, foo: :bar)
                 end
  end
end
