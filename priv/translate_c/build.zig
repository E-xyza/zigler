pub const Translator = @import("build/Translator.zig");
pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const skip_translate = b.option(bool, "skip-translate", "Main test suite skips translate tests") orelse false;
    const skip_run_translated = b.option(bool, "skip-run-translated", "Main test suite skips run-translated tests") orelse false;
    const test_cross_targets = b.option(bool, "test-cross-targets", "Include cross-translation targets in the test cases") orelse false;
    const test_filters = b.option([]const []const u8, "test-filter", "Skip tests that do not match any filter") orelse &[0][]const u8{};
    const use_llvm = b.option(bool, "llvm", "Use LLVM backend to generate aro executable");
    const debug_allocations = b.option(bool, "debug-allocations", "Collect detailed debug info for all allocations (slow)") orelse false;

    const options = b.addOptions();
    options.addOption(bool, "debug_allocations", debug_allocations);

    const aro = b.dependency("aro", .{
        .target = target,
        .optimize = optimize,
    });

    const c_builtins = b.addModule("c_builtins", .{
        .root_source_file = b.path("lib/c_builtins.zig"),
    });

    const helpers = b.addModule("helpers", .{
        .root_source_file = b.path("lib/helpers.zig"),
    });

    const translate_c_module = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    translate_c_module.addImport("aro", aro.module("aro"));
    translate_c_module.addImport("helpers", helpers);
    translate_c_module.addImport("c_builtins", c_builtins);
    translate_c_module.addImport("build_options", options.createModule());

    if (target.result.os.tag == .windows) {
        translate_c_module.linkSystemLibrary("advapi32", .{});
    }

    const translate_c_exe = b.addExecutable(.{
        .name = "translate-c",
        .root_module = translate_c_module,
        .use_llvm = use_llvm,
        .use_lld = use_llvm,
    });

    b.installDirectory(.{
        .source_dir = aro.path("include"),
        .install_dir = .prefix,
        .install_subdir = "include",
    });
    b.installDirectory(.{
        .source_dir = b.path("lib"),
        .install_dir = .prefix,
        .install_subdir = "lib",
    });
    b.installArtifact(translate_c_exe);

    // Re-expose the path to Aro's "resource directory" (which is actually just the repo root). This
    // is needed to correctly discover its builtin 'include' directory when `translate-c` is invoked
    // programmatically.
    b.addNamedLazyPath("aro_resource_dir", aro.path(""));

    const translator_conf: Translator.TranslateCConfig = .{
        .exe = translate_c_exe,
        .aro_resource_dir = aro.path(""),
        .c_builtins = c_builtins,
        .helpers = helpers,
    };

    const run_step = b.step("run", "Run translate-c");
    run_step.dependOn(step: {
        const run_cmd = b.addRunArtifact(translate_c_exe);
        run_cmd.step.dependOn(b.getInstallStep());
        run_cmd.addPassthruArgs();
        break :step &run_cmd.step;
    });

    const fmt_dirs = b.pathList(&.{ "build", "build.zig", "src", "lib", "test", "examples" });

    const fmt_step = b.step("fmt", "Modify source files in place to have conforming formatting");
    fmt_step.dependOn(&b.addFmt(.{ .paths = fmt_dirs }).step);

    const test_fmt_step = b.step("test-fmt", "Check source files having conforming formatting");
    test_fmt_step.dependOn(&b.addFmt(.{ .paths = fmt_dirs, .check = true }).step);

    const test_unit_step = b.step("test-unit", "Run unit tests");
    test_unit_step.dependOn(step: {
        const unit_tests_mod = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
        });
        unit_tests_mod.addImport("aro", aro.module("aro"));
        unit_tests_mod.addImport("helpers", helpers);
        unit_tests_mod.addImport("c_builtins", c_builtins);
        break :step &b.addRunArtifact(b.addTest(.{ .root_module = unit_tests_mod, .filters = test_filters })).step;
    });

    const test_macros_step = b.step("test-macros", "Run macro tests");
    test_macros_step.dependOn(step: {
        const macro_tests_mod = b.createModule(.{
            .root_source_file = b.path("test/macros.zig"),
            .target = target,
            .optimize = optimize,
        });
        macro_tests_mod.addImport("macros.h", Translator.initInner(b, translator_conf, .{
            .c_source_file = b.path("test/macros.h"),
            .target = target,
            .optimize = optimize,
        }).mod);
        macro_tests_mod.addImport("macros_not_utf8.h", Translator.initInner(b, translator_conf, .{
            .c_source_file = b.path("test/macros_not_utf8.h"),
            .target = target,
            .optimize = optimize,
        }).mod);
        break :step &b.addRunArtifact(b.addTest(.{ .root_module = macro_tests_mod, .filters = test_filters })).step;
    });

    const test_translate_step = b.step("test-translate", "Run the C translation tests");
    const test_run_translated_step = b.step("test-run-translated", "Run the run-translated-c tests");
    @import("test/cases.zig").lowerCases(
        b,
        translator_conf,
        target,
        optimize,
        test_filters,
        test_cross_targets,
        test_translate_step,
        test_run_translated_step,
    );

    const test_step = b.step("test", "Run all tests");
    test_step.dependOn(test_fmt_step);
    test_step.dependOn(test_unit_step);
    test_step.dependOn(test_macros_step);
    if (!skip_translate) test_step.dependOn(test_translate_step);
    if (!skip_run_translated) test_step.dependOn(test_run_translated_step);
}

const std = @import("std");
