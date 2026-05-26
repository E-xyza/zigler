defmodule ZiglerTest.Erlang.SpecTest do
  use ZiglerTest.IntegrationCase, async: true
  alias ZiglerTest.Compiler

  @moduletag :erlang
  @test_file to_charlist(Path.join(__DIR__, "src/erlang_spec_test"))

  test "erlang module has specs generated" do
    Compiler.compile_erlang(@test_file, [:debug_info])

    # Get the beam file location
    beam_file = :code.which(:erlang_spec_test)

    # Read the abstract code from the beam file
    {:ok, {:erlang_spec_test, [{:abstract_code, {:raw_abstract_v1, forms}}]}} =
      :beam_lib.chunks(beam_file, [:abstract_code])

    # Find all spec attributes
    specs =
      forms
      |> Enum.filter(fn
        {:attribute, _, :spec, _} -> true
        _ -> false
      end)
      |> Enum.map(fn {:attribute, _, :spec, spec} -> spec end)

    # Check that we have a spec for add/2
    assert Enum.any?(specs, fn
             {{:add, 2}, _} -> true
             _ -> false
           end),
           "Expected spec for add/2, got: #{inspect(specs)}"

    # Check that we have a spec for get_bool/1
    assert Enum.any?(specs, fn
             {{:get_bool, 1}, _} -> true
             _ -> false
           end),
           "Expected spec for get_bool/1, got: #{inspect(specs)}"

    # Check that we have a spec for void_fn/0
    assert Enum.any?(specs, fn
             {{:void_fn, 0}, _} -> true
             _ -> false
           end),
           "Expected spec for void_fn/0, got: #{inspect(specs)}"
  end
end
