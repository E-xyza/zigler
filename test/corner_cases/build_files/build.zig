const std = @import("std");

const BuildMode = enum { nif_lib, sema };

pub fn build(b: *std.Build) void {
    const resolved_target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const mode = b.option(BuildMode, "zigler-mode", "Build either the nif library or the sema analysis module") orelse .nif_lib;

    switch (mode) {
        .nif_lib => {
            const erl_nif = b.createModule(.{
                .root_source_file = .{ .cwd_relative = "/home/ityonemo/code/zigler/.elixir_ls/build/test/lib/zigler/priv/beam/erl_nif.zig" },
                .imports = &[_]std.Build.Module.Import{},

                .link_libc = true,
            });

            erl_nif.addSystemIncludePath(.{ .cwd_relative = "/home/ityonemo/.asdf/installs/erlang/28.0.2/erts-16.0.2/include" });

            const beam = b.createModule(.{
                .root_source_file = .{ .cwd_relative = "/home/ityonemo/code/zigler/.elixir_ls/build/test/lib/zigler/priv/beam/beam.zig" },
                .imports = &[_]std.Build.Module.Import{
                    .{ .name = "erl_nif", .module = erl_nif },
                },
            });

            const attributes = b.createModule(.{
                .root_source_file = .{ .cwd_relative = "attributes.zig" },
                .imports = &[_]std.Build.Module.Import{},
            });

            const module = b.createModule(.{
                .root_source_file = .{ .cwd_relative = "/home/ityonemo/code/zigler/test/corner_cases/build_files/module.zig" },
                .imports = &[_]std.Build.Module.Import{},
            });

            const nif = b.createModule(.{
                .root_source_file = .{ .cwd_relative = "/home/ityonemo/code/zigler/test/corner_cases/.Elixir.ZiglerTest.CornerCases.BuildZigOverrideTest.zig" },
                .imports = &[_]std.Build.Module.Import{
                    .{ .name = "erl_nif", .module = erl_nif },

                    .{ .name = "beam", .module = beam },

                    .{ .name = "attributes", .module = attributes },

                    .{ .name = "module", .module = module },
                },

                .link_libc = true,
            });

            const nif_shim = b.createModule(.{ .root_source_file = .{ .cwd_relative = "module.zig" }, .imports = &[_]std.Build.Module.Import{
                .{ .name = "beam", .module = beam },

                .{ .name = "erl_nif", .module = erl_nif },

                .{ .name = "nif", .module = nif },
            }, .target = resolved_target, .optimize = optimize });

            const lib = b.addLibrary(.{ .name = "Elixir.ZiglerTest.CornerCases.BuildZigOverrideTest", .linkage = .dynamic, .version = .{ .major = 0, .minor = 15, .patch = 1 }, .root_module = nif_shim });

            lib.linker_allow_shlib_undefined = true;

            // the native backend still causes segfaults, so we must disable it for now.
            lib.use_llvm = true;

            b.installArtifact(lib);
        },
        .sema => {
            const erl_nif = b.createModule(.{
                .root_source_file = .{ .cwd_relative = "/home/ityonemo/code/zigler/.elixir_ls/build/test/lib/zigler/priv/beam/stub_erl_nif.zig" },
                .imports = &[_]std.Build.Module.Import{},
            });

            const beam = b.createModule(.{
                .root_source_file = .{ .cwd_relative = "/home/ityonemo/code/zigler/.elixir_ls/build/test/lib/zigler/priv/beam/beam.zig" },
                .imports = &[_]std.Build.Module.Import{
                    .{ .name = "erl_nif", .module = erl_nif },
                },
            });

            const attributes = b.createModule(.{
                .root_source_file = .{ .cwd_relative = "attributes.zig" },
                .imports = &[_]std.Build.Module.Import{},
            });

            const module = b.createModule(.{
                .root_source_file = .{ .cwd_relative = "/home/ityonemo/code/zigler/test/corner_cases/build_files/module.zig" },
                .imports = &[_]std.Build.Module.Import{},
            });

            const nif = b.createModule(.{
                .root_source_file = .{ .cwd_relative = "/home/ityonemo/code/zigler/test/corner_cases/.Elixir.ZiglerTest.CornerCases.BuildZigOverrideTest.zig" },
                .imports = &[_]std.Build.Module.Import{
                    .{ .name = "erl_nif", .module = erl_nif },

                    .{ .name = "beam", .module = beam },

                    .{ .name = "attributes", .module = attributes },

                    .{ .name = "module", .module = module },
                },

                .link_libc = true,
            });

            const sema = b.createModule(.{ .root_source_file = .{ .cwd_relative = "/home/ityonemo/code/zigler/.elixir_ls/build/test/lib/zigler/priv/beam/sema.zig" }, .imports = &[_]std.Build.Module.Import{
                .{ .name = "nif", .module = nif },
            }, .target = resolved_target, .optimize = optimize });

            const sema_exe = b.addExecutable(.{ .name = "sema", .root_module = sema });

            b.installArtifact(sema_exe);

            const sema_run_cmd = b.addRunArtifact(sema_exe);
            if (b.args) |args| sema_run_cmd.addArgs(args);
            const sema_step = b.step("sema", "Run sema");
            sema_step.dependOn(&sema_run_cmd.step);
        },
    }
}
