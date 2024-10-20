defmodule Zig.Target do
  @moduledoc false

  # interfaces for cross-compilation logic.

  # this function primarily exists to support Nerves deployments, though
  # it is possible to set an arbitrary cross-compilation target using a
  # setting in your `use Zig` directive.  This selects the architecture
  # by checking your "CC" environment variable, which is in turn set by
  # Nerves, then adjusts gcc's machine type to a string which allows zig to
  # select the appropriate cross-compilation settings and libc.

  defstruct [:arch, :os, :abi]

  @type t :: %__MODULE__{
          arch: String.t(),
          os: String.t(),
          abi: String.t()
        }

  # obtains the target from the
  @spec resolve() :: nil | t
  def resolve do
    arch = System.get_env("TARGET_ARCH")
    os = System.get_env("TARGET_OS")
    abi = System.get_env("TARGET_ABI")

    if arch && os && abi do
      %__MODULE__{arch: arch, os: os, abi: abi}
    end
  end

  def for_builder(target \\ resolve()) do
    if target do
      ".{.default_target=.{.cpu_arch = .#{target.arch}, .os_tag = .#{target.os}, .abi = .#{target.abi}}}"
    else
      ".{}"
    end
  end
end
