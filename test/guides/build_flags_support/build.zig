const std = @import("std");

const BuildMode = enum { nif_lib, sema };

// Helper to strip surrounding quotes from paths (needed for Windows paths with spaces)
fn stripQuotes(path: []const u8) []const u8 {
    if (path.len >= 2 and path[0] == '"' and path[path.len - 1] == '"') {
        return path[1 .. path.len - 1];
    }
    return path;
}

pub fn build(b: *std.Build) void {
    const resolved_target = b.standardTargetOptions(.{});
    const host_target = b.graph.host;
    const optimize: std.builtin.OptimizeMode = .Debug;
    const error_tracing = true;

    // ERTS and zigler paths (auto-injected by zigler as -D flags)
    // stripQuotes handles Windows paths with spaces that are quoted on the command line
    const erts_include = stripQuotes(b.option([]const u8, "erts_include", "ERTS include path") orelse @panic("erts_include required"));
    const erl_nif_header = stripQuotes(b.option([]const u8, "erl_nif_header", "Path to erl_nif.h (or erl_nif_win.h on Windows)") orelse @panic("erl_nif_header required"));
    const erl_nif_win_path = stripQuotes(b.option([]const u8, "erl_nif_win_path", "Path to Windows erl_nif compatibility headers") orelse @panic("erl_nif_win_path required"));
    const zigler_priv = stripQuotes(b.option([]const u8, "zigler_priv", "Path to zigler priv directory") orelse @panic("zigler_priv required"));
    const module_root = stripQuotes(b.option([]const u8, "module_root", "Path to module source directory") orelse @panic("module_root required"));

    // Custom build flag option - this is what we're testing
    const custom_message = b.option([]const u8, "custom_message", "A custom message to embed") orelse "default";

    // Create build_options module above switch so it's available to both branches
    const build_options = b.addOptions();
    build_options.addOption([]const u8, "custom_message", custom_message);

    const mode = b.option(BuildMode, "zigler-mode", "Build either the nif library or the sema analysis module") orelse .nif_lib;

    // Helper to build paths relative to zigler_priv
    const beam_path = std.fs.path.join(b.allocator, &.{ zigler_priv, "beam" }) catch @panic("OOM");

    switch (mode) {
        .nif_lib => {
            // Translate erl_nif.h to Zig module
            const erl_translate_c = b.addTranslateC(.{
                .root_source_file = .{ .cwd_relative = erl_nif_header },
                .target = resolved_target,
                .optimize = optimize,
                .link_libc = true,
            });
            erl_translate_c.addSystemIncludePath(.{ .cwd_relative = erts_include });
            erl_translate_c.addSystemIncludePath(.{ .cwd_relative = erl_nif_win_path });
            const erl_module = erl_translate_c.createModule();

            const erl_nif_path = std.fs.path.join(b.allocator, &.{ beam_path, "erl_nif.zig" }) catch @panic("OOM");
            const erl_nif = b.createModule(.{
                .root_source_file = .{ .cwd_relative = erl_nif_path },
                .imports = &[_]std.Build.Module.Import{},
                .link_libc = true,
            });

            erl_nif.addSystemIncludePath(.{ .cwd_relative = erts_include });
            erl_nif.addSystemIncludePath(.{ .cwd_relative = erl_nif_win_path });
            erl_nif.addImport("erl", erl_module);

            const beam_zig_path = std.fs.path.join(b.allocator, &.{ beam_path, "beam.zig" }) catch @panic("OOM");
            const beam = b.createModule(.{
                .root_source_file = .{ .cwd_relative = beam_zig_path },
                .imports = &[_]std.Build.Module.Import{
                    .{ .name = "erl_nif", .module = erl_nif },
                },
            });

            const attributes = b.createModule(.{
                .root_source_file = .{ .cwd_relative = "attributes.zig" },
                .imports = &[_]std.Build.Module.Import{},
            });

            const nif_path = std.fs.path.join(b.allocator, &.{ module_root, ".Elixir.ZiglerTest.Guides.BuildFlagsCustomTest.zig" }) catch @panic("OOM");
            const nif = b.createModule(.{
                .root_source_file = .{ .cwd_relative = nif_path },
                .imports = &[_]std.Build.Module.Import{
                    .{ .name = "erl_nif", .module = erl_nif },
                    .{ .name = "beam", .module = beam },
                    .{ .name = "attributes", .module = attributes },
                },
                .link_libc = true,
            });

            // Add build_options so zig code can @import("build_options")
            nif.addOptions("build_options", build_options);

            const nif_shim = b.createModule(.{
                .root_source_file = .{ .cwd_relative = "module.zig" },
                .imports = &[_]std.Build.Module.Import{
                    .{ .name = "beam", .module = beam },
                    .{ .name = "erl_nif", .module = erl_nif },
                    .{ .name = "nif", .module = nif },
                },
                .target = resolved_target,
                .optimize = optimize,
                .error_tracing = error_tracing,
            });

            const lib = b.addLibrary(.{
                .name = "Elixir.ZiglerTest.Guides.BuildFlagsCustomTest",
                .linkage = .dynamic,
                .version = .{ .major = 0, .minor = 16, .patch = 0 },
                .root_module = nif_shim,
            });

            lib.linker_allow_shlib_undefined = true;
            lib.use_llvm = true;

            if (resolved_target.result.cpu.arch == .x86) {
                lib.link_z_notext = true;
            }

            b.installArtifact(lib);
        },
        .sema => {
            const stub_path = std.fs.path.join(b.allocator, &.{ beam_path, "stub_erl_nif.zig" }) catch @panic("OOM");
            const erl_nif = b.createModule(.{
                .root_source_file = .{ .cwd_relative = stub_path },
                .imports = &[_]std.Build.Module.Import{},
            });

            const beam_zig_path = std.fs.path.join(b.allocator, &.{ beam_path, "beam.zig" }) catch @panic("OOM");
            const beam = b.createModule(.{
                .root_source_file = .{ .cwd_relative = beam_zig_path },
                .imports = &[_]std.Build.Module.Import{
                    .{ .name = "erl_nif", .module = erl_nif },
                },
            });

            const attributes = b.createModule(.{
                .root_source_file = .{ .cwd_relative = "attributes.zig" },
                .imports = &[_]std.Build.Module.Import{},
            });

            const nif_path = std.fs.path.join(b.allocator, &.{ module_root, ".Elixir.ZiglerTest.Guides.BuildFlagsCustomTest.zig" }) catch @panic("OOM");
            const nif = b.createModule(.{
                .root_source_file = .{ .cwd_relative = nif_path },
                .imports = &[_]std.Build.Module.Import{
                    .{ .name = "erl_nif", .module = erl_nif },
                    .{ .name = "beam", .module = beam },
                    .{ .name = "attributes", .module = attributes },
                },
                .link_libc = true,
            });

            // Add build_options for sema
            nif.addOptions("build_options", build_options);

            const sema_path = std.fs.path.join(b.allocator, &.{ beam_path, "sema.zig" }) catch @panic("OOM");
            const sema = b.createModule(.{
                .root_source_file = .{ .cwd_relative = sema_path },
                .imports = &[_]std.Build.Module.Import{
                    .{ .name = "nif", .module = nif },
                },
                .target = host_target,
                .optimize = optimize,
                .error_tracing = error_tracing,
            });

            const sema_exe = b.addExecutable(.{
                .name = "sema",
                .root_module = sema,
            });

            b.installArtifact(sema_exe);

            const sema_run_cmd = b.addRunArtifact(sema_exe);
            if (b.args) |args| sema_run_cmd.addArgs(args);
            const sema_step = b.step("sema", "Run sema");
            sema_step.dependOn(&sema_run_cmd.step);
        },
    }
}
