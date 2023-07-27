defmodule ZiglerTest.Sema.ResourceTest do
  use ExUnit.Case, async: true

  use Zig, otp_app: :zigler, precompile: false

  alias Zig.Sema
  alias Zig.Type
  alias Zig.Type.Resource
  alias Zig.Type.Struct

  setup_all do
    sema_result =
      __DIR__
      |> Path.join(".#{__MODULE__}.zig")
      |> Sema.run_sema!(__MODULE__)

    {:ok, sema_result}
  end

  ~Z"""
  const beam = @import("beam");
  const StructResource = beam.Resource(T, @import("root"), .{});

  pub const T = struct {
    payload: u64,
  };

  pub fn res(resource: StructResource) T {
    return resource.unpack();
  }
  """

  test "resource in parameter", %{functions: [function]} do
    assert %Type.Function{
             name: :res,
             arity: 1,
             params: [
               %Resource{
                 name: "Resource(nif.T,root,.{.Callbacks = null})"
               }
             ],
             return: %Struct{name: "T"}
           } = function
  end
end
