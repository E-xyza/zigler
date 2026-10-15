const std = @import("std");
const Translator = @import("../build/Translator.zig");
const mem = std.mem;

const cross_targets: []const []const u8 = &.{
    "aarch64-freebsd",
    "arm-freebsd",
    "powerpc64le-freebsd",
    "riscv64-freebsd",
    "x86_64-freebsd",

    "aarch64-linux",
    "arm-linux",
    "loongarch64-linux",
    "powerpc64le-linux",
    "riscv64-linux",
    "s390x-linux",
    "x86_64-linux",

    "aarch64-macos",

    "aarch64-netbsd",
    "arm-netbsd",
    "x86_64-netbsd",

    "aarch64-openbsd",
    "arm-openbsd",
    "riscv64-openbsd",
    "x86_64-openbsd",

    "aarch64-windows",
    "x86_64-windows",
};

pub fn lowerCases(
    b: *std.Build,
    translator_conf: Translator.TranslateCConfig,
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
    test_filters: []const []const u8,
    test_cross_targets: bool,
    test_translate_step: *std.Build.Step,
    test_run_translated_step: *std.Build.Step,
) void {
    const io = b.graph.io;
    var targets_buf: [cross_targets.len + 1]std.Build.ResolvedTarget = undefined;
    const targets: []const std.Build.ResolvedTarget = targets: {
        targets_buf[0] = target;
        if (!test_cross_targets) break :targets targets_buf[0..1];
        for (cross_targets, targets_buf[1..]) |query_str, *resolved| {
            const query = std.Target.Query.parse(.{ .arch_os_abi = query_str }) catch unreachable;
            resolved.* = b.resolveTargetQuery(query);
        }
        break :targets &targets_buf;
    };

    var dir = b.root.openDir(io, "test/cases", .{ .iterate = true }) catch |err| {
        const fail_step = b.addFail(b.fmt("unable to open test/cases: {s}", .{@errorName(err)}));
        test_translate_step.dependOn(&fail_step.step);
        test_run_translated_step.dependOn(&fail_step.step);
        return;
    };
    defer dir.close(io);

    var it = dir.walk(b.allocator) catch |err| std.debug.panic("failed to walk cases: {s}", .{@errorName(err)});
    while (it.next(io) catch |err| {
        std.debug.panic("failed to walk cases: {s}", .{@errorName(err)});
    }) |entry| {
        if (entry.kind != .file) continue;
        const case = caseFromFile(b, entry) catch |err|
            std.debug.panic("failed to process case '{s}': {s}", .{ entry.path, @errorName(err) });
        if (test_filters.len > 0) {
            for (test_filters) |filter| {
                if (std.mem.find(u8, case.name, filter) != null) break;
            } else continue;
        }

        const source_file = b.addWriteFiles().add("tmp.c", case.input);

        const case_targets = if (case.target) |*t| t[0..1] else targets;
        for (case_targets) |case_target| {
            if (case.skip_vector_index) continue;
            if (case.skip_windows and case_target.result.os.tag == .windows) {
                continue;
            }
            const name_and_triple = b.fmt("{s} {s}", .{
                case_target.query.zigTriple(b.graph.arena) catch @panic("OOM"),
                case.name,
            });

            var options: Translator.Options = .{
                .name = name_and_triple,
                .c_source_file = source_file,
                .target = case_target,
                .optimize = optimize,
            };

            // var args_array: std.ArrayList([]const u8) = .empty;
            // defer args_array.deinit(b.graph.arena);
            if (case.args) |args| {
                var arg_it = std.mem.tokenizeScalar(u8, args, ' ');
                while (arg_it.next()) |arg| {
                    if (mem.eql(u8, arg, "-fdefault-init")) {
                        options.default_init = true;
                    } else if (mem.cutPrefix(u8, arg, "-fstrict-flex-arrays=")) |rest| {
                        if (rest.len != 1 or rest[0] < '0' or rest[0] > '3') {
                            @panic("Bad case: -fstrict-flex-arrays= requires a value of '0', '1', '2', or '3'");
                        }
                        options.strict_flex_arrays = @fromBackingInt(@intCast(rest[0] - '0'));
                    } else {
                        @panic("Unhandled arg in cases");
                    }
                }
            }
            const translator: Translator = .initInner(b, translator_conf, options);
            switch (case.kind) {
                .translate => |output| {
                    const check_file = b.addCheckFile(translator.output_file, .{ .expected_matches = output });
                    check_file.step.name = b.fmt("check-translated {s}", .{name_and_triple});
                    test_translate_step.dependOn(&check_file.step);
                },
                .run => |output| {
                    const exe = b.addExecutable(.{
                        .name = case.name,
                        .root_module = translator.mod,
                        .max_rss = 1_000_000_000,
                    });
                    const run = b.addRunArtifact(exe);
                    run.step.name = b.fmt("run-translated {s}", .{name_and_triple});
                    run.expectStdOutEqual(output);
                    run.skip_foreign_checks = true;
                    test_run_translated_step.dependOn(&run.step);
                },
            }
        }
    }
}

const Case = struct {
    name: []const u8,
    /// This is an override; usually `null`.
    target: ?std.Build.ResolvedTarget,
    input: []const u8,
    args: ?[]const u8,
    kind: Kind,
    skip_windows: bool,
    skip_vector_index: bool,

    const Expect = enum { pass, fail };

    const Kind = union(enum) {
        /// Translate the input, run it and check that it
        /// outputs the expected text.
        run: []const u8,
        /// Translate the input and check that it contains
        /// the expected lines of code.
        translate: []const []const u8,
    };
};

fn caseFromFile(b: *std.Build, entry: std.Io.Dir.Walker.Entry) !Case {
    const max_file_size = 10 * 1024 * 1024;
    const src = try entry.dir.readFileAlloc(b.graph.io, entry.basename, b.allocator, .limited(max_file_size));

    const input, const manifest = blk: {
        var start: ?usize = null;
        const bytes = std.mem.trimEnd(u8, src, " \t\n");
        var cursor = bytes.len;
        while (true) : (cursor -= 1) {
            while (cursor > 0 and bytes[cursor - 1] != '\n') cursor -= 1;

            if (std.mem.startsWith(u8, bytes[cursor..], "//")) {
                start = cursor;
            } else break;
        }
        const manifest_start = start orelse return error.TestManifestMissing;
        break :blk .{ bytes[0..manifest_start], bytes[manifest_start..] };
    };

    var target: ?std.Target.Query = null;
    var skip_windows = false;
    var skip_vector_index = false;
    var args: ?[]const u8 = null;

    var it = std.mem.tokenizeScalar(u8, manifest, '\n');

    const kind = kind: {
        const line = it.next() orelse return error.TestManifestMissingType;
        const trimmed = std.mem.trim(u8, line[2..], " \t");
        break :kind std.meta.stringToEnum(std.meta.Tag(Case.Kind), trimmed) orelse {
            std.log.warn("invalid test case type: {s}", .{trimmed});
            return error.TestManifestInvalidType;
        };
    };

    while (it.next()) |line| {
        const trimmed = std.mem.trim(u8, line[2..], " \t");

        if (trimmed.len == 0) break; // Start of trailing data.

        const key_raw, const value_raw = std.mem.cutScalar(u8, trimmed, '=') orelse {
            std.debug.print("{s}: missing value for config option: {s}\n", .{ entry.basename, trimmed });
            return error.TestManifestMissingValue;
        };
        const key = std.mem.trimEnd(u8, key_raw, " \t");
        const value = std.mem.trimStart(u8, value_raw, " \t");
        if (std.mem.eql(u8, key, "target")) {
            target = try .parse(.{ .arch_os_abi = value });
        } else if (std.mem.eql(u8, key, "skip_windows")) {
            skip_windows = std.mem.eql(u8, value, "true");
        } else if (std.mem.eql(u8, key, "skip_vector_index")) {
            skip_vector_index = std.mem.eql(u8, value, "true");
        } else if (std.mem.eql(u8, key, "args")) {
            args = value;
        } else return error.InvalidTestConfigOption;
    }

    return .{
        .name = std.fs.path.stem(entry.basename),
        .target = if (target) |q| b.resolveTargetQuery(q) else null,
        .input = input,
        .kind = switch (kind) {
            .run => .{ .run = try trailing(b.allocator, &it) },
            .translate => .{ .translate = try trailingSplit(b.allocator, &it) },
        },
        .skip_windows = skip_windows,
        .skip_vector_index = skip_vector_index,
        .args = args,
    };
}

fn trailing(arena: std.mem.Allocator, it: *std.mem.TokenIterator(u8, .scalar)) ![]const u8 {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(arena);
    while (it.next()) |line| {
        if (line.len < 3) continue;
        const trimmed = line[3..];
        if (buf.items.len != 0) try buf.append(arena, '\n');
        try buf.appendSlice(arena, trimmed);
    }
    return try buf.toOwnedSlice(arena);
}

fn trailingSplit(arena: std.mem.Allocator, it: *std.mem.TokenIterator(u8, .scalar)) ![]const []const u8 {
    var out: std.ArrayList([]const u8) = .empty;
    defer out.deinit(arena);
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(arena);

    while (it.next()) |line| {
        if (line.len <= 3) {
            if (buf.items.len != 0) {
                try out.append(arena, try buf.toOwnedSlice(arena));
            }
            continue;
        }
        const trimmed = line[3..];
        if (buf.items.len != 0) try buf.append(arena, '\n');
        try buf.appendSlice(arena, trimmed);
    }
    if (buf.items.len != 0) {
        try out.append(arena, try buf.toOwnedSlice(arena));
    }
    return try out.toOwnedSlice(arena);
}
