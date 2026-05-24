# Custom Build Flags

Zigler allows you to pass custom `-D` flags to the Zig build system. This enables
advanced use cases like injecting compile-time configuration, mobile cross-compilation,
or static linking.

## Using build_flags

The `build_flags` option passes flags directly to `zig build`:

```elixir
defmodule BuildFlagsExample do
  use Zig,
    otp_app: :my_app,
    build_flags: ["-Dmy_option=hello"]
end
```

For the flags to have any effect, your `build.zig` must define matching options using
`b.option()`. This requires providing a custom `build.zig` via `build_files_dir`.

## Complete Example: Compile-Time Configuration

This example shows how to pass a string via `-D` flag and have it appear in your
NIF's output.

### Step 1: Generate a build.zig template

Use `dump_build_zig: true` to see zigler's generated build.zig:

```elixir
defmodule MyModule do
  use Zig, otp_app: :my_app, dump_build_zig: true

  ~Z"""
  pub fn hello() u32 { return 42; }
  """
end
```

The build.zig will be in your module's staging directory (check the compile logs).

### Step 2: Create a custom build.zig with your option

Copy the generated `build.zig` and `build.zig.zon` to a directory (e.g., `zig_build/`),
then modify the build.zig to add a custom option.

Here's an annotated version showing the key additions (marked with `// NEW:`):

```zig
const std = @import("std");

const BuildMode = enum { nif_lib, sema };

pub fn build(b: *std.Build) void {
    const resolved_target = b.standardTargetOptions(.{});
    const host_target = b.graph.host;
    const optimize: std.builtin.OptimizeMode = .Debug;
    const error_tracing = true;

    // NEW: Define a custom string option
    const custom_message = b.option(
        []const u8,
        "custom_message",
        "A custom message to embed in the NIF"
    ) orelse "default message";

    const mode = b.option(BuildMode, "zigler-mode", "Build mode") orelse .nif_lib;

    switch (mode) {
        .nif_lib => {
            const target_for_modules = resolved_target;

            // ... erl_translate_c setup ...
            // ... erl_nif module setup ...
            // ... beam module setup ...
            // ... attributes module setup ...

            const nif = b.createModule(.{
                .root_source_file = .{ .cwd_relative = "path/to/your.zig" },
                .imports = &[_]std.Build.Module.Import{
                    .{ .name = "erl_nif", .module = erl_nif },
                    .{ .name = "beam", .module = beam },
                    .{ .name = "attributes", .module = attributes },
                },
                .link_libc = true,
            });

            // NEW: Create an options module to expose the custom value
            const build_options = b.addOptions();
            build_options.addOption([]const u8, "custom_message", custom_message);

            // NEW: Add the options module to the nif so it can @import("build_options")
            nif.addOptions("build_options", build_options);

            // ... nif_shim setup ...
            // ... lib setup ...
        },
        .sema => {
            const target_for_modules = host_target;

            // ... sema module setup ...

            const nif = b.createModule(.{
                // ... nif setup for sema ...
            });

            // NEW: Also add options for sema so the code compiles during analysis
            const build_options = b.addOptions();
            build_options.addOption([]const u8, "custom_message", custom_message);
            nif.addOptions("build_options", build_options);

            // ... rest of sema setup ...
        },
    }
}
```

### Step 3: Use the option in your Zig code

```elixir
defmodule MyApp.ConfigurableNif do
  use Zig,
    otp_app: :my_app,
    build_files_dir: "./zig_build",
    build_flags: ["-Dcustom_message=Hello from build flags!"]

  ~Z"""
  const build_options = @import("build_options");

  pub fn get_message() []const u8 {
      return build_options.custom_message;
  }
  """
end
```

Now calling `MyApp.ConfigurableNif.get_message()` returns `"Hello from build flags!"`.

### Step 4: Dynamic configuration from Mix

You can compute build flags at compile time:

```elixir
defmodule MyApp.VersionedNif do
  @version Mix.Project.config()[:version]

  use Zig,
    otp_app: :my_app,
    build_files_dir: "./zig_build",
    build_flags: ["-Dcustom_message=v#{@version}"]

  ~Z"""
  const build_options = @import("build_options");

  pub fn version() []const u8 {
      return build_options.custom_message;
  }
  """
end
```

## Use Cases

### Mobile Cross-Compilation

For iOS builds, you may need to add Apple SDK include paths. Add this to your
custom `build.zig`:

```zig
const apple_sdkroot = b.option(
    []const u8,
    "apple_sdkroot",
    "Path to Apple SDK"
) orelse null;

// In nif_lib section, after erl_translate_c setup:
if (apple_sdkroot) |sdk| {
    erl_translate_c.addSystemIncludePath(.{ .cwd_relative = sdk });
}
```

Then use:

```elixir
use Zig,
  otp_app: :my_app,
  build_files_dir: "./zig_build",
  build_flags: ["-Dapple_sdkroot=/Applications/Xcode.app/.../iPhoneOS.sdk"]
```

### Static Linking

For embedded systems without dynamic loading, modify your `build.zig`:

```zig
const nif_linkage = b.option(
    std.builtin.LinkMode,
    "nif_linkage",
    "Library linkage mode"
) orelse .dynamic;

// Change the library creation:
const lib = b.addLibrary(.{
    .name = "MyNif",
    .linkage = nif_linkage,  // was: .dynamic
    // ...
});
```

Then use:

```elixir
use Zig,
  otp_app: :my_app,
  build_files_dir: "./zig_build",
  build_flags: ["-Dnif_linkage=static"]
```

### Environment-Specific Builds

```elixir
@build_flags case Mix.env() do
  :prod -> ["-Dlog_level=error"]
  :dev  -> ["-Dlog_level=debug"]
  :test -> ["-Dlog_level=warn"]
end

use Zig,
  otp_app: :my_app,
  build_files_dir: "./zig_build",
  build_flags: @build_flags
```

## Tips

- Use `dump_build_zig: true` to see zigler's generated build.zig as a starting point
- When using `build_files_dir`, you must provide both `build.zig` and `build.zig.zon`
- Remember to add options in both `nif_lib` and `sema` branches
- Build options are resolved at compile time, not runtime
- String values in `-D` flags don't need extra quoting
