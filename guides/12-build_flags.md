# Custom Build Flags

Zigler allows you to pass custom `-D` flags to the Zig build system. This enables
advanced use cases like injecting compile-time configuration.

## Overview

The `build_flags` option passes flags directly to `zig build`. To use custom flags,
you must provide a custom `build.zig` via `build_files_dir` that defines matching
`b.option()` calls. Zig 0.16+ rejects unknown `-D` options.

## Setting Up Custom Build Flags

### Step 1: Generate a build.zig template

First, use `dump_build_zig: true` to print zigler's generated build.zig to the console:

    defmodule MyModule do
      use Zig, otp_app: :my_app, dump_build_zig: true
      # ...
    end

Copy the printed `build.zig` and `build.zig.zon` to a directory (e.g., `zig_build/`).

### Step 2: Add your custom option to build.zig

Modify the build.zig to define your option and create a build_options module:

```zig
pub fn build(b: *std.Build) void {
    // ... standard setup ...

    // Define a custom string option
    const custom_message = b.option(
        []const u8,
        "custom_message",
        "A custom message to embed in the NIF"
    ) orelse "default message";

    // Create build_options module above switch so it's available to both branches
    const build_options = b.addOptions();
    build_options.addOption([]const u8, "custom_message", custom_message);

    const mode = b.option(BuildMode, "zigler-mode", "Build mode") orelse .nif_lib;

    switch (mode) {
        .nif_lib => {
            // ... module setup ...

            const nif = b.createModule(.{ /* ... */ });

            // Add build_options so zig code can @import("build_options")
            nif.addOptions("build_options", build_options);

            // ... rest of nif_lib setup ...
        },
        .sema => {
            // ... module setup ...

            const nif = b.createModule(.{ /* ... */ });

            // Also add to sema if your zig code uses @import("build_options")
            nif.addOptions("build_options", build_options);

            // ... rest of sema setup ...
        },
    }
}
```

### Step 3: Use the option in your module

Configure your module with `build_files_dir` pointing to your custom build files,
and pass your flags via `build_flags`. Access the value in zig via `@import("build_options")`:

```elixir
defmodule ZiglerTest.Guides.BuildFlagsCustomTest do
  use ExUnit.Case, async: true

  use Zig,
    otp_app: :zigler,
    build_files_dir: "build_flags_support",
    build_flags: ["-Dcustom_message=hello_from_test"]

  ~Z"""
  const build_options = @import("build_options");
  const beam = @import("beam");

  pub fn get_custom_message() beam.term {
      return beam.make(build_options.custom_message, .{});
  }
  """

  test "build_flags passes custom option to zig" do
    assert "hello_from_test" == get_custom_message()
  end
end
#module
```

## Tips

- Use `dump_build_zig: true` to see zigler's generated build.zig as a starting point
- When using `build_files_dir`, you must provide both `build.zig` and `build.zig.zon`
- Define `build_options` above the switch so it's available to both branches
- Call `nif.addOptions()` in `nif_lib`, and also in `sema` if your zig code uses `@import("build_options")`
- Build options are resolved at compile time, not runtime
