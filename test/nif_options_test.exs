defmodule ZiglerTest.NifOptionsTest do
  use ExUnit.Case, async: true

  alias Zig.Nif

  @default_opts [
    module: __MODULE__,
    file: __ENV__.file,
    line: __ENV__.line,
    module_code_path: "module.path",
    zig_code_path: "zig.path"
  ]

  defp make_nif(opts) do
    Nif.new(
      {:my_nif, opts ++ @default_opts},
      Map.new(@default_opts ++ [keystack: [:my_nif, :nifs], cleanup: true])
    )
  end

  test "empty is viable" do
    assert %Nif{} = make_nif([])
  end

  describe "export option" do
    test "defaults to true" do
      assert %{export: true} = make_nif([])
    end

    test "accepts boolean" do
      assert %{export: true} = make_nif(export: true)
    end

    test "rejects non-boolean" do
      assert_raise CompileError,
                   "test/nif_options_test.exs:9: option `nifs > my_nif > export` must be a boolean, got: `1`",
                   fn ->
                     make_nif(export: 1)
                   end
    end
  end

  describe "concurrency option" do
    test "defaults to synchronous" do
      assert %{concurrency: Zig.Nif.Synchronous} = make_nif([])
    end

    test "accepts single atoms" do
      assert %{concurrency: Zig.Nif.Synchronous} = make_nif([:synchronous])
      assert %{concurrency: Zig.Nif.Threaded} = make_nif([:threaded])
      assert %{concurrency: Zig.Nif.Yielding} = make_nif([:yielding])
      assert %{concurrency: Zig.Nif.DirtyCpu} = make_nif([:dirty_cpu])
      assert %{concurrency: Zig.Nif.DirtyIo} = make_nif([:dirty_io])
    end

    test "accepts valid options" do
      assert %{concurrency: Zig.Nif.Synchronous} = make_nif(concurrency: :synchronous)
      assert %{concurrency: Zig.Nif.Threaded} = make_nif(concurrency: :threaded)
      assert %{concurrency: Zig.Nif.Yielding} = make_nif(concurrency: :yielding)
      assert %{concurrency: Zig.Nif.DirtyCpu} = make_nif(concurrency: :dirty_cpu)
      assert %{concurrency: Zig.Nif.DirtyIo} = make_nif(concurrency: :dirty_io)
    end

    test "rejects invalid options" do
      assert_raise CompileError,
                   "test/nif_options_test.exs:9: option `nifs > my_nif > concurrency` must be one of `:dirty_cpu`, `:dirty_io`, `:synchronous`, `:threaded`, `:yielding`, got: `:invalid`",
                   fn ->
                     make_nif(concurrency: :invalid)
                   end
    end
  end

  describe "spec option" do
    test "defaults to true" do
      assert %{spec: true} = make_nif([])
    end

    test "is false for :nospec" do
      assert %{spec: false} = make_nif([:nospec])
    end

    test "accepts boolean" do
      assert %{spec: false} = make_nif(spec: false)
    end

    test "rejects non-boolean" do
      assert_raise CompileError,
                   "test/nif_options_test.exs:9: option `nifs > my_nif > spec` must be boolean, got: `1`",
                   fn ->
                     make_nif(spec: 1)
                   end
    end
  end

  describe "impl option" do
    test "defaults to nil" do
      assert %{impl: nil} = make_nif([])
    end

    test "accepts module name" do
      assert %{impl: MyModule} = make_nif(impl: MyModule)
    end

    test "rejects on non-atom" do
      assert_raise CompileError,
                   "test/nif_options_test.exs:9: option `nifs > my_nif > impl` must be a module or `true`, got: `1`",
                   fn ->
                     make_nif(impl: 1)
                   end
    end
  end

  describe "alias option" do
    test "defaults to nil" do
      assert %{alias: nil} = make_nif([])
    end

    test "accepts an atom" do
      assert %{alias: :bar} = make_nif(alias: :bar)
    end

    test "rejects on self" do
      assert_raise CompileError,
                   "test/nif_options_test.exs:9: option `nifs > my_nif > alias` may not be the same as the nif name `my_nif`",
                   fn ->
                     make_nif(alias: :my_nif)
                   end
    end

    test "rejects on non-atom" do
      assert_raise CompileError,
                   "test/nif_options_test.exs:9: option `nifs > my_nif > alias` must be an atom, got: `1`",
                   fn ->
                     make_nif(alias: 1)
                   end
    end
  end

  describe "allocator option" do
    test "defaults to nil" do
      assert %{allocator: nil} = make_nif([])
    end

    test "accepts atom" do
      assert %{allocator: :my_allocator} = make_nif(allocator: :my_allocator)
    end

    test "rejects on non-atom" do
      assert_raise CompileError,
                   "test/nif_options_test.exs:9: option `nifs > my_nif > allocator` must be an atom, got: `1`",
                   fn ->
                     make_nif(allocator: 1)
                   end
    end
  end

  describe "leak check option" do
    test "defaults to false" do
      assert %{leak_check: false} = make_nif([])
    end

    test "is true for :leak_check" do
      assert %{leak_check: true} = make_nif([:leak_check])
    end

    test "accepts boolean" do
      assert %{leak_check: true} = make_nif(leak_check: true)
    end

    test "rejects non-boolean" do
      assert_raise CompileError,
                   "test/nif_options_test.exs:9: option `nifs > my_nif > leak_check` must be boolean, got: `1`",
                   fn ->
                     make_nif(leak_check: 1)
                   end
    end
  end

  describe "cleanup option" do
    test "defaults to true" do
      assert %{cleanup: true} = make_nif([])
    end

    test "is false for :noclean" do
      assert %{cleanup: false} = make_nif([:noclean])
    end

    test "accepts boolean" do
      assert %{cleanup: false} = make_nif(cleanup: false)
    end

    test "rejects non-boolean" do
      assert_raise CompileError,
                   "test/nif_options_test.exs:9: option `nifs > my_nif > cleanup` must be boolean, got: `1`",
                   fn ->
                     make_nif(cleanup: 1)
                   end
    end
  end

  describe "return option" do
    test "defaults to empty, with cleanup based on nif parameters" do
      assert %{return: %{cleanup: true}} = make_nif([])
      assert %{return: %{cleanup: false}} = make_nif(cleanup: false)
      assert %{return: %{cleanup: false}} = make_nif([:noclean])
    end

    test "noclean or cleanup: false overrides cleanup" do
      assert %{return: %{cleanup: false}} = make_nif(return: [:noclean])
      assert %{return: %{cleanup: false}} = make_nif(return: [cleanup: false])
    end

    test "cleanup: true can override noclean" do
      assert %{return: %{cleanup: true}} = make_nif([:noclean, return: [cleanup: true]])
    end

    @as ~w[binary list integer map]a
    test "accepts different forms for as" do
      Enum.each(@as, fn as ->
        assert %{return: %{cleanup: true, as: ^as}} = make_nif(return: as)
        assert %{return: %{cleanup: true, as: ^as}} = make_nif(return: [as])
        assert %{return: %{cleanup: true, as: ^as}} = make_nif(return: [as: as])
      end)
    end

    test "rejects invalid as option" do
      assert_raise CompileError,
                   "test/nif_options_test.exs:9: option `nifs > my_nif > return > as` has an invalid type specification (must be `:binary`, `:list`, `:map`, or `{:list, type}`, `{:map, key: type}`), got: `:foo`",
                   fn ->
                     make_nif(return: [as: :foo])
                   end
    end

    test "list and map details are supported" do
      assert %{return: %{cleanup: true, as: {:list, :binary}}} =
               make_nif(return: {:list, :binary})

      assert %{return: %{cleanup: true, as: {:map, foo: :binary}}} =
               make_nif(return: {:map, foo: :binary})
    end

    test "nested details must be valid" do
      assert_raise CompileError,
                   "test/nif_options_test.exs:9: option `nifs > my_nif > return > list` has an invalid type specification (must be `:binary`, `:list`, `:map`, or `{:list, type}`, `{:map, key: type}`), got: `:foo`",
                   fn ->
                     make_nif(return: {:list, :foo})
                   end

      assert_raise CompileError,
                   "test/nif_options_test.exs:9: option `nifs > my_nif > return > map > foo` has an invalid type specification (must be `:binary`, `:list`, `:map`, or `{:list, type}`, `{:map, key: type}`), got: `:foo`",
                   fn ->
                     make_nif(return: {:map, foo: :foo})
                   end

      assert_raise CompileError,
                   "test/nif_options_test.exs:9: option `nifs > my_nif > return > map` has an invalid map type specification (map parameter must be a keyword list of atoms as keys and types as values), got: `:foo`",
                   fn ->
                     make_nif(return: {:map, :foo})
                   end

      assert_raise CompileError,
                   "test/nif_options_test.exs:9: option `nifs > my_nif > return > map` has an invalid map type specification (map parameter must be a keyword list of atoms as keys and types as values), got: `[1, 2, 3]`",
                   fn ->
                     make_nif(return: {:map, [1, 2, 3]})
                   end

      assert_raise CompileError,
                   "test/nif_options_test.exs:9: option `nifs > my_nif > return > list > map > foo` has an invalid type specification (must be `:binary`, `:list`, `:map`, or `{:list, type}`, `{:map, key: type}`), got: `:foo`",
                   fn ->
                     make_nif(return: {:list, {:map, foo: :foo}})
                   end
    end

    test "in_out can be an atom" do
      assert %{return: %{cleanup: true, in_out: :foo}} = make_nif(return: [in_out: :foo])
    end

    test "in_out is rejected if it's not an atom" do
      assert_raise CompileError,
                   "test/nif_options_test.exs:9: nif option `in_out` must be an atom, got: `1`",
                   fn ->
                     make_nif(return: [in_out: 1])
                   end
    end

    test "error can be an atom" do
      assert %{return: %{cleanup: true, error: :foo}} = make_nif(return: [error: :foo])
    end

    test "error is rejected if it's not an atom" do
      assert_raise CompileError,
                   "test/nif_options_test.exs:9: nif option `error` must be a module, got: `1`",
                   fn ->
                     make_nif(return: [error: 1])
                   end
    end

    test "length can be integer" do
      assert %{return: %{cleanup: true, length: 10}} = make_nif(return: [length: 10])
    end

    test "length can be argument spec" do
      assert %{return: %{cleanup: true, length: {:arg, 0}}} =
               make_nif(return: [length: {:arg, 0}])
    end

    test "length can't be anything else" do
      assert_raise CompileError,
                   "test/nif_options_test.exs:9: nif option `length` must be a non-negative integer or an argument spec, got: `:foo`",
                   fn ->
                     make_nif(return: [length: :foo])
                   end
    end
  end

  alias Zig.Parameter

  describe "params option" do
    test "defaults to empty" do
      assert %{params: %{}} = make_nif([])
    end

    test "you can set it to a params map" do
      assert %{params: %{0 => %Parameter{cleanup: true, in_out: false}}} =
               make_nif(params: %{0 => []})
    end

    test "cleanup adopts value from outer options" do
      assert %{params: %{0 => %Parameter{cleanup: true}}} =
               make_nif(params: %{0 => []}, cleanup: true)
    end

    test "you can't set it to anything else" do
      assert_raise CompileError,
                   "test/nif_options_test.exs:9: option `nifs > my_nif > params` must be a map, got: `:foo`",
                   fn ->
                     make_nif(params: :foo)
                   end
    end

    test "params map keys must be integers" do
      assert_raise CompileError,
                   "test/nif_options_test.exs:9: option `nifs > my_nif > params` must be a map with non-negative integer keys, got: `:foo`",
                   fn ->
                     make_nif(params: %{foo: []})
                   end
    end

    test "params map can set cleanup with precedence over the nif default" do
      assert %{params: %{0 => %Parameter{cleanup: false}}} =
               make_nif(params: %{0 => [cleanup: false]}, cleanup: true)

      assert %{params: %{0 => %Parameter{cleanup: false}}} =
               make_nif(params: %{0 => [:noclean]}, cleanup: true)
    end

    test "non-boolean cleanup values are rejected" do
      assert_raise CompileError,
                   "test/nif_options_test.exs:9: option `nifs > my_nif > 0 > params > cleanup` must be boolean, got: `1`",
                   fn ->
                     make_nif(params: %{0 => [cleanup: 1]})
                   end
    end

    test "params map can set in_out" do
      assert %{params: %{0 => %Parameter{in_out: true}}} =
               make_nif(params: %{0 => [in_out: true]})

      assert %{params: %{0 => %Parameter{in_out: true}}} =
               make_nif(params: %{0 => [:in_out]})
    end

    test "non-boolean in_out values are rejected" do
      assert_raise CompileError,
                   "test/nif_options_test.exs:9: option `nifs > my_nif > 0 > params > in_out` must be boolean, got: `1`",
                   fn ->
                     make_nif(params: %{0 => [in_out: 1]})
                   end
    end

    test "other values in the list are rejected" do
      assert_raise CompileError,
                   "test/nif_options_test.exs:9: option `nifs > my_nif > 0 > params` found an invalid term in the options list, got: `:foo`",
                   fn ->
                     make_nif(params: %{0 => [:foo]})
                   end

      assert_raise CompileError,
                   "test/nif_options_test.exs:9: option `nifs > my_nif` was supplied the invalid option `foo`",
                   fn ->
                     make_nif(params: %{0 => [foo: :bar]})
                   end
    end
  end

  describe "invalid options" do
    test "atom wonky" do
      assert_raise CompileError,
                   "test/nif_options_test.exs:9: option `nifs > my_nif` found an invalid term in the options list, got: `:foobar`",
                   fn ->
                     make_nif([:foobar])
                   end
    end
  end

  describe "arity" do
    test "single number" do
      assert %{arity: [1]} = make_nif(arity: 1)
    end

    test "list of numbers" do
      assert %{arity: [1, 3]} = make_nif(arity: [1, 3])
    end

    test "naked range" do
      assert %{arity: [1, 2, 3]} = make_nif(arity: 1..3)
    end

    test "list of number and range" do
      assert %{arity: [1, 2, 3, 5, 6]} = make_nif(arity: [1, 2..3, 5..6])
    end

    test "unacceptable content" do
      assert_raise CompileError,
                   "test/nif_options_test.exs:9: option `nifs > my_nif > arity` must be a non-negative integer, range or a list of those, got: `:foo`",
                   fn ->
                     make_nif(arity: [1, 2..3, 5..6, :foo])
                   end
    end
  end
end
