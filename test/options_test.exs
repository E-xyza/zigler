defmodule ZiglerTest.OptionsTest do
  use ExUnit.Case, async: true

  defp make_module(opts), do: Zig.Module.new(opts ++ [nifs: [:foo], language: :elixir], __ENV__)

  describe "for the module_code_path option" do
    test "string is ok" do
      assert %Zig.Module{} = make_module(otp_app: :zigler, module_code_path: "foobar")
    end

    test "iodata is ok" do
      assert %Zig.Module{} = make_module(otp_app: :zigler, module_code_path: ["foo" | "bar"])
    end

    test "non-path raises" do
      assert_raise CompileError,
                   "test/options_test.exs:4: `module_code_path` option must be a path, got: `:foo`",
                   fn ->
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
      assert_raise CompileError,
                   "test/options_test.exs:4: `zig_code_path` option must be a path, got: `:foo`",
                   fn ->
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

    test "src accepts basic string forms" do
      local_file = Path.join(__DIR__, "my_file.c")

      assert %{c: %{src: [{^local_file, []}]}} =
               make_module(otp_app: :zigler, c: [src: "my_file.c"])

      priv_file = Path.join(:code.priv_dir(:zigler), "my_file.c")

      assert %{c: %{src: [{^priv_file, []}]}} =
               make_module(otp_app: :zigler, c: [src: {:priv, "my_file.c"}])
    end

    test "src accepts string with string options" do
      local_dir = Path.join(__DIR__, "my_dir")

      assert %{c: %{src: [{^local_dir, ["foo", "bar"]}]}} =
               make_module(otp_app: :zigler, c: [src: {"my_dir", ["foo", "bar"]}])
    end

    test "src not a string list" do
      assert_raise CompileError,
                   "test/options_test.exs:4: `c` option `src` must be a string, `{string, [string]}`, `{:priv, string}`, `{:priv, string, [string]}`, `{:system, string}`, `{:system, string, [string]}`, or a list of those, got: `:moop`",
                   fn ->
                     make_module(otp_app: :zigler, c: [src: :moop])
                   end
    end
  end

  describe "the dir option" do
    test "accepts iodata" do
      assert %{dir: "foo/bar"} = make_module(otp_app: :zigler, dir: ["foo", "/" | "bar"])
    end

    test "rejects non-iodata" do
      assert_raise CompileError,
                   "test/options_test.exs:4: `dir` option must be a path, got: `:foo`",
                   fn ->
                     make_module(otp_app: :zigler, dir: :foo)
                   end
    end
  end

  describe "the easy_c option" do
    test "accepts iodata" do
      assert %{easy_c: "foo/bar"} = make_module(otp_app: :zigler, easy_c: ["foo", "/" | "bar"])
    end

    test "rejects non-iodata" do
      assert_raise CompileError,
                   "test/options_test.exs:4: `easy_c` option must be a path, got: `:foo`",
                   fn ->
                     make_module(otp_app: :zigler, easy_c: :foo)
                   end
    end
  end

  describe "release_mode" do
    test "is ok with fast, small, safe, debug, env" do
      Enum.each(~w[fast small safe debug env]a, fn mode ->
        assert %{release_mode: ^mode} = make_module(otp_app: :zigler, release_mode: mode)
      end)
    end

    test "rejects other" do
      assert_raise CompileError,
                   "test/options_test.exs:4: option `release_mode` must be one of `:debug`, `:safe`, `:fast`, `:small`, `:env`, got: `:foo`",
                   fn ->
                     make_module(otp_app: :zigler, release_mode: :foo)
                   end
    end
  end

  describe "ignore" do
    test "is ok with a list of atoms" do
      assert %{ignore: [:foo, :bar]} =
               make_module(otp_app: :zigler, ignore: [:foo, :bar])
    end

    test "is ok with a single atom" do
      assert %{ignore: [:foo]} =
               make_module(otp_app: :zigler, ignore: :foo)
    end

    test "is not ok with something else" do
      assert_raise CompileError,
                   "test/options_test.exs:4: option `ignore` must be a list of atoms, got: `\"foo\"`",
                   fn ->
                     make_module(otp_app: :zigler, ignore: ["foo"])
                   end
    end
  end

  describe "packages" do
    test "accepts name-path-deps" do
      assert %{packages: [foo: {"bar", [:baz, :quux]}]} =
               make_module(otp_app: :zigler, packages: [foo: {"bar", [:baz, :quux]}])
    end

    test "path must be a string" do
      assert_raise CompileError,
                   "test/options_test.exs:4: option `packages > foo` must be a tuple of the form `{path, [deps...]}`, got: `:bar` for path",
                   fn ->
                     make_module(otp_app: :zigler, packages: [foo: {:bar, [:baz, :quux]}])
                   end
    end

    test "deps must be a list of atoms" do
      assert_raise CompileError,
                   "test/options_test.exs:4: option `packages > foo` must have a list of atoms for deps, got: `\"baz\"`",
                   fn ->
                     make_module(otp_app: :zigler, packages: [foo: {"bar", "baz"}])
                   end
    end

    test "must be a keyword list" do
      assert_raise CompileError,
                   "test/options_test.exs:4: option `packages` must be a list of package specifications, got: `\"foo\"`",
                   fn ->
                     make_module(otp_app: :zigler, packages: "foo")
                   end

      assert_raise CompileError,
                   "test/options_test.exs:4: option `packages` must be a list of package specifications, got: `\"bar\"`",
                   fn ->
                     make_module(otp_app: :zigler, packages: ["bar"])
                   end
    end

    test "payload must be a tuple" do
      assert_raise CompileError,
                   "test/options_test.exs:4: option `packages > foo` must be a tuple of the form `{path, [deps...]}`, got: `\"bar\"`",
                   fn ->
                     make_module(otp_app: :zigler, packages: [foo: "bar"])
                   end
    end
  end

  describe "resources" do
    test "is ok with a list of atoms" do
      assert %{resources: [:foo, :bar]} =
               make_module(otp_app: :zigler, resources: [:foo, :bar])
    end

    test "is ok with a single atom" do
      assert %{resources: [:foo]} =
               make_module(otp_app: :zigler, resources: :foo)
    end

    test "is not ok with something else" do
      assert_raise CompileError,
                   "test/options_test.exs:4: option `resources` must be a list of atoms, got: `\"foo\"`",
                   fn ->
                     make_module(otp_app: :zigler, resources: ["foo"])
                   end
    end
  end

  describe "callbacks" do
    test "can have on_load as an atom" do
      assert %{callbacks: [on_load: :on_load]} =
               make_module(otp_app: :zigler, callbacks: [:on_load])
    end

    test "on_load can be keyword atom" do
      assert %{callbacks: [on_load: :fun]} =
               make_module(otp_app: :zigler, callbacks: [on_load: :fun])
    end

    test "on_load rejects anything else" do
      assert_raise CompileError,
                   "test/options_test.exs:4: option `callbacks > on_load` must be an atom, got: `[\"foo\"]`",
                   fn ->
                     make_module(otp_app: :zigler, callbacks: [on_load: ["foo"]])
                   end
    end

    test "can have on_upgrade as an atom" do
      assert %{callbacks: [on_upgrade: :on_upgrade]} =
               make_module(otp_app: :zigler, callbacks: [:on_upgrade])
    end

    test "on_upgrade can be keyword atom" do
      assert %{callbacks: [on_upgrade: :fun]} =
               make_module(otp_app: :zigler, callbacks: [on_upgrade: :fun])
    end

    test "on_upgrade rejects anything else" do
      assert_raise CompileError,
                   "test/options_test.exs:4: option `callbacks > on_upgrade` must be an atom, got: `[\"foo\"]`",
                   fn ->
                     make_module(otp_app: :zigler, callbacks: [on_upgrade: ["foo"]])
                   end
    end

    test "can have on_unload as an atom" do
      assert %{callbacks: [on_unload: :on_unload]} =
               make_module(otp_app: :zigler, callbacks: [:on_unload])
    end

    test "on_unload can be keyword atom" do
      assert %{callbacks: [on_unload: :fun]} =
               make_module(otp_app: :zigler, callbacks: [on_unload: :fun])
    end

    test "on_unload rejects anything else" do
      assert_raise CompileError,
                   "test/options_test.exs:4: option `callbacks > on_unload` must be an atom, got: `[\"foo\"]`",
                   fn ->
                     make_module(otp_app: :zigler, callbacks: [on_unload: ["foo"]])
                   end
    end

    test "anything else is not allowed" do
      assert_raise CompileError,
                   "test/options_test.exs:4: option `callbacks` must be a list of callback specifications, got: `:foo`",
                   fn ->
                     make_module(otp_app: :zigler, callbacks: :foo)
                   end
    end
  end

  describe "cleanup" do
    test "cleanup gets placed in default nif opts" do
      assert %{default_nif_opts: [cleanup: true]} =
               make_module(otp_app: :zigler, cleanup: true)
    end

    test "cleanup must be a boolean" do
      assert_raise CompileError,
                   "test/options_test.exs:4: option `cleanup` must be a boolean, got: `:foo`",
                   fn ->
                     make_module(otp_app: :zigler, cleanup: :foo)
                   end
    end
  end

  describe "leak_check" do
    test "leak_check gets placed in default nif opts" do
      assert %{default_nif_opts: [leak_check: true]} =
               make_module(otp_app: :zigler, leak_check: true)
    end

    test "leak_check must be a boolean" do
      assert_raise CompileError,
                   "test/options_test.exs:4: option `leak_check` must be a boolean, got: `:foo`",
                   fn ->
                     make_module(otp_app: :zigler, leak_check: :foo)
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
