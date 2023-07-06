defmodule ZiglerTest.Sema.ResourceTest do
  use ExUnit.Case, async: true

  use Zig, otp_app: :zigler, precompile: false

  alias Zig.Nif
  alias Zig.Type
  alias Zig.Type.Resource
  alias Zig.Type.Struct
  alias Zig.Sema

  setup_all do
    file = Path.join(__DIR__, ".#{__MODULE__}.zig")

    sema_map =
      __MODULE__
      |> Sema.analyze_file!(
        nifs: {:auto, []},
        default_options: Nif.default_options(),
        file: file,
        manifest: []
      )
      |> Enum.map(fn {name, opts} -> {name, Keyword.fetch!(opts, :type)} end)

    {:ok, sema_map}
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

  test "resource in parameter", %{res: res} do
    assert %Type.Function{
             name: :res,
             arity: 1,
             params: [
               %Resource{
                 name: "Resource(nif.T,root,.{.Callbacks = null})"
               }
             ],
             return: %Struct{name: "T"}
           } = res
  end
end
