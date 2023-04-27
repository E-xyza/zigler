defmodule ZiglerTest.Sema.ResourceTest do
  use ExUnit.Case, async: true

  use Zig, otp_app: :zigler, precompile: false

  alias Zig.Type
  alias Zig.Type.Resource
  alias Zig.Type.Struct
  alias Zig.Sema

  setup_all do
    sema_map =
      __MODULE__
      |> Sema.analyze_file!(nifs: :all)
      |> Map.new(&{&1.name, &1})

    {:ok, sema_map}
  end

  ~Z"""
  const beam = @import("beam");
  pub const resource = beam.resources(@import("root"));
  const Resource = beam.Resource;

  pub const T = struct {
    payload: u64,
  };

  pub fn res(value: Resource(T)) T {
    return resource.unpack(value);
  }
  """

  test "a basic function can be found", %{res: res} do
    assert %Type.Function{
             name: :res,
             arity: 1,
             params: [%Resource{payload: %Zig.Type.Struct{name: "T"}}],
             return: %Struct{name: "T"}
           } = res
  end
end
