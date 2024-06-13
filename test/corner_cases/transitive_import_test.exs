defmodule ZiglerTest.CornerCases.TransitiveImportTest do
  use ExUnit.Case, async: true
  use Zig, otp_app: :zigler

  ~Z"""
  const single = @import("single.zig");
  const S = single.S;
  const D = single.D;
  const C = single.C;
  pub fn identity_single(value: S) S { return value; }
  pub fn identity_double(value: D) D {return value;}
  pub fn identity_circular(value: C) C {return value;}
  """

  describe "singly transitive" do
    test "types work" do
      assert %{foo: 47} = identity_single(%{foo: 47})
    end

    test "files are labeled as external resources" do
      assert "test/corner_cases/single.zig" in __MODULE__.__info__(:attributes)[:external_resource]
    end
  end

  describe "doubly transitive" do
    test "types work" do
      assert %{foo: 47} = identity_double(%{foo: 47})
    end

    test "files are labeled as external resources" do
      assert "test/corner_cases/double.zig" in __MODULE__.__info__(:attributes)[:external_resource]
    end
  end

  describe "circularly transitive" do
    test "types work" do
      assert %{foo: 47} = identity_circular(%{foo: 47})
    end
  end
end