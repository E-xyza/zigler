const std = @import("std");
const assert = std.debug.assert;
const mem = std.mem;
const Allocator = std.mem.Allocator;
const process = std.process;
const fatal = std.process.fatal;
const Io = std.Io;
const PkgConfig = std.zig.PkgConfig;

const aro = @import("aro");

const build_options = @import("build_options");

const Translator = @import("Translator.zig");

var safe_allocator: std.heap.DebugAllocator(.{
    .stack_trace_frames = if (build_options.debug_allocations and std.debug.sys_can_stack_trace) 10 else 0,
    .resize_stack_traces = build_options.debug_allocations,
    // A unique value so that when a default-constructed
    // GeneralPurposeAllocator is incorrectly passed to testing allocator, or
    // vice versa, panic occurs.
    .canary = @truncate(0x8cffe9c54f0bcc72),
}) = .{};

pub fn main(init: process.Init) !void {
    const gpa = if (@import("builtin").link_libc)
        std.heap.c_allocator
    else
        safe_allocator.allocator();
    defer if (!@import("builtin").link_libc) {
        _ = safe_allocator.deinit();
    };

    var arena_instance = std.heap.ArenaAllocator.init(gpa);
    defer arena_instance.deinit();
    const arena = arena_instance.allocator();

    const io = init.io;
    const args = try init.minimal.args.toSlice(arena);

    var stderr_buf: [1024]u8 = undefined;
    var stderr = Io.File.stderr().writer(io, &stderr_buf);
    var diagnostics: aro.Diagnostics = .{
        .output = .{ .to_writer = .{
            .mode = Io.Terminal.Mode.detect(io, stderr.file, false, false) catch .no_color,
            .writer = &stderr.interface,
        } },
    };

    var comp = try aro.Compilation.init(.{
        .gpa = gpa,
        .arena = arena,
        .io = io,
        .diagnostics = &diagnostics,
        .environ_map = init.environ_map,
    });
    defer comp.deinit();

    var driver: aro.Driver = .{ .comp = &comp, .diagnostics = &diagnostics, .aro_name = args[0] };
    defer driver.deinit();

    var toolchain: aro.Toolchain = .{ .driver = &driver };
    defer toolchain.deinit();

    var stdout_buf: [512]u8 = undefined;
    var stdout = Io.File.stdout().writer(io, &stdout_buf);
    translate(&driver, &toolchain, init.environ_map, &stdout.interface, args) catch |err| switch (err) {
        error.FatalError => process.exit(1),
        error.WriteFailed => return stdout.err.?,
        else => |e| return e,
    };
    if (comp.diagnostics.errors != 0) process.exit(1);
    return process.cleanExit(io);
}

pub const usage =
    \\Usage {s}: [options] file [CC options]
    \\
    \\Options:
    \\  --help                      Print this message
    \\  --version                   Print translate-c version
    \\  --libc=<file>               Provide a file which specifies libc paths
    \\  -fmodule-libs               Import libraries as modules
    \\  -fno-module-libs            (default) Install libraries next to output file
    \\  -fpub-static                (default) Translate static functions as pub
    \\  -fno-pub-static             Do not translate static functions as pub
    \\  -ffunc-bodies               (default) Translate function bodies
    \\  -fno-func-bodies            Do not translate function bodies
    \\  -fkeep-macro-literals       (default) Preserve macro names for literals
    \\  -fno-keep-macro-literals    Do not preserve macro names for literals
    \\  -fdefault-init              Default initialize struct fields
    \\  -fno-default-init           (default) Do not default initialize struct fields
    \\  -fstrict-flex-arrays=<n>    Control when to treat a trailing array as a flexible array member (default: 2)
    \\                                0: any trailing array
    \\                                1: size [0]/[1]/[]
    \\                                2: size [0]/[]
    \\                                3: [] only
    \\
    \\
;

fn translate(
    d: *aro.Driver,
    tc: *aro.Toolchain,
    environ_map: *const process.Environ.Map,
    stdout: *Io.Writer,
    args: []const [:0]const u8,
) !void {
    const arena = d.comp.arena;
    const gpa = d.comp.gpa;
    const io = d.comp.io;

    var module_libs = false;
    var pub_static = true;
    var func_bodies = true;
    var keep_macro_literals = true;
    var default_init = false;
    var strict_flex_arrays: Translator.StrictFlexArraysLevel = .@"2";
    var target_query: std.Target.Query = .{};
    var link_libc = false;
    var link_libcpp = false;
    var link_libunwind = false;
    var libc_paths_file: ?[]const u8 = null;

    var system_libs: std.ArrayList(SystemLib) = .empty;
    var any_want_pkg_conf = false;
    var any_force_pkg_conf = false;
    var optimize_mode: std.lang.OptimizeMode = .Debug;
    var opt_zig_lib_path: ?[]const u8 = null;

    var aro_args: std.ArrayList([]const u8) = try .initCapacity(arena, args.len);

    aro_args.appendAssumeCapacity(args[0]);

    for (args[1..], 1..) |arg, i| {
        if (mem.eql(u8, arg, "--help")) {
            try stdout.print(usage, .{args[0]});
            try stdout.flush();
            return;
        } else if (mem.eql(u8, arg, "--version")) {
            // TODO add version
            try stdout.writeAll("0.0.0-dev\n");
            try stdout.flush();
            return;
        } else if (mem.cutPrefix(u8, arg, "--libc=")) |rest| {
            libc_paths_file = rest;
        } else if (mem.eql(u8, arg, "-fmodule-libs")) {
            module_libs = true;
        } else if (mem.eql(u8, arg, "-fno-module-libs")) {
            module_libs = false;
        } else if (mem.eql(u8, arg, "-fpub-static")) {
            pub_static = true;
        } else if (mem.eql(u8, arg, "-fno-pub-static")) {
            pub_static = false;
        } else if (mem.eql(u8, arg, "-ffunc-bodies")) {
            func_bodies = true;
        } else if (mem.eql(u8, arg, "-fno-func-bodies")) {
            func_bodies = false;
        } else if (mem.eql(u8, arg, "-fkeep-macro-literals")) {
            keep_macro_literals = true;
        } else if (mem.eql(u8, arg, "-fno-keep-macro-literals")) {
            keep_macro_literals = false;
        } else if (mem.eql(u8, arg, "-fdefault-init")) {
            default_init = true;
        } else if (mem.eql(u8, arg, "-fno-default-init")) {
            default_init = false;
        } else if (mem.cutPrefix(u8, arg, "-fstrict-flex-arrays=")) |rest| {
            if (rest.len != 1 or rest[0] < '0' or rest[0] > '3') {
                return d.fatal("-fstrict-flex-arrays= requires a value of '0', '1', '2', or '3'", .{});
            }
            strict_flex_arrays = @fromBackingInt(@intCast(rest[0] - '0'));
        } else if (mem.cutPrefix(u8, arg, "--target=")) |rest| {
            target_query = std.zig.parseTargetQueryOrReportFatalError(arena, .{
                .arch_os_abi = rest,
            });
            try aro_args.append(arena, arg);
        } else if (mem.cutPrefix(u8, arg, "-mcpu=")) |rest| {
            try aro_args.ensureUnusedCapacity(arena, 2);
            aro_args.appendAssumeCapacity("-mcpu");
            aro_args.appendAssumeCapacity(rest);
        } else if (mem.cutPrefix(u8, arg, "-o=")) |rest| {
            try aro_args.ensureUnusedCapacity(arena, 2);
            aro_args.appendAssumeCapacity("-o");
            aro_args.appendAssumeCapacity(rest);
        } else if (mem.cutPrefix(u8, arg, "-O=")) |rest| {
            optimize_mode = std.meta.stringToEnum(std.lang.OptimizeMode, rest) orelse
                fatal("bad optimize mode: {s}", .{rest});
        } else if (mem.eql(u8, arg, "-lc")) {
            link_libc = true;
        } else if (mem.eql(u8, arg, "-lc++")) {
            link_libcpp = true;
        } else if (mem.eql(u8, arg, "-lunwind")) {
            link_libunwind = true;
        } else if (mem.cutPrefix(u8, arg, "-l=")) |rest| {
            try system_libs.append(arena, .{
                .options = .{
                    .needed = (rest[0] - '0') != 0,
                    .weak = (rest[1] - '0') != 0,
                    .use_pkg_config = @fromBackingInt(@intCast(rest[2] - '0')),
                    .preferred_link_mode = @fromBackingInt(@intCast(rest[3] - '0')),
                    .search_strategy = @fromBackingInt(@intCast(rest[4] - '0')),
                },
                .name = rest[6..],
            });
            switch (system_libs.last().?.options.use_pkg_config) {
                .no => {},
                .yes => any_want_pkg_conf = true,
                .force => any_force_pkg_conf = true,
            }
        } else if (mem.cutPrefix(u8, arg, "--zig-lib=")) |rest| {
            opt_zig_lib_path = rest;
        } else if (mem.eql(u8, arg, "--")) {
            try aro_args.appendSlice(arena, args[i + 1 ..]);
            break;
        } else {
            fatal("unrecognized translate-c argument: {s}", .{arg});
        }
    }

    const zig_lib_path = opt_zig_lib_path orelse fatal("missing --zig-lib=[path] argument", .{});

    try aro_args.append(arena, "-nostdlibinc");

    switch (optimize_mode) {
        .Debug => {
            try aro_args.append(arena, "-O0");
        },
        .ReleaseSafe => {
            try aro_args.append(arena, "-O2");
            try aro_args.append(arena, "-D_FORTIFY_SOURCE=2");
        },
        .ReleaseFast => {
            try aro_args.append(arena, "-O2");
            try aro_args.append(arena, "-DNDEBUG");
        },
        .ReleaseSmall => {
            try aro_args.append(arena, "-Os");
            try aro_args.append(arena, "-DNDEBUG");
        },
    }

    const target = std.zig.resolveTargetQueryOrFatal(io, target_query);

    if (link_libc) {
        if (target.isGnuLibC()) {
            const target_version = target.os.versionRange().gnuLibCVersion().?;
            const glibc_minor_define = try std.fmt.allocPrint(arena, "-D__GLIBC_MINOR__={d}", .{
                target_version.minor,
            });
            try aro_args.append(arena, glibc_minor_define);
        } else if (target.isMinGW()) {
            try aro_args.append(arena, "-D__MSVCRT_VERSION__=0xE00"); // use ucrt

            const minver: u16 = @truncate(@backingInt(target.os.versionRange().windows.min) >> 16);
            try aro_args.append(
                arena,
                try std.fmt.allocPrint(arena, "-D_WIN32_WINNT=0x{x:0>4}", .{minver}),
            );

            // MinGW-w64's inline functions in headers (e.g. `fabs`), which are emitted with `linkonce_odr`
            // linkage, sometimes cause duplicate symbol errors due to us providing the same symbols with
            // `weak` linkage in compiler-rt or libzigc. So just disable them. Besides, they undermine the
            // goal of moving more libc code to Zig, and they're also just kind of unnecessary since LLVM is
            // perfectly capable of recognizing and optimizing libcalls.
            try aro_args.append(arena, "-D__CRT__NO_INLINE");
        } else if (target.isFreeBSDLibC()) {
            // https://docs.freebsd.org/en/books/porters-handbook/versions
            const min_ver = target.os.version_range.semver.min;
            try aro_args.append(arena, try std.fmt.allocPrint(arena, "-D__FreeBSD_version={d}", .{
                // We don't currently respect the minor and patch components. This wouldn't be particularly
                // helpful because our abilists file only tracks major FreeBSD releases, so the link-time stub
                // symbols would be inconsistent with header declarations.
                min_ver.major * 100_000 + 500,
            }));
        } else if (target.isNetBSDLibC()) {
            const min_ver = target.os.version_range.semver.min;
            try aro_args.append(arena, try std.fmt.allocPrint(arena, "-D__NetBSD_Version__={d}", .{
                // We don't currently respect the patch component. This wouldn't be particularly helpful because
                // our abilists file only tracks major and minor NetBSD releases, so the link-time stub symbols
                // would be inconsistent with header declarations.
                (min_ver.major * 100_000_000) + (min_ver.minor * 1_000_000),
            }));
        } else if (target.isOpenBSDLibC()) {
            const min_ver = target.os.version_range.semver.min;
            // The macro in sys/param.h doesn't have the leading underscores, but we don't want to pollute the
            // global namespace in all compilation units. So we use leading underscores and modify sys/param.h
            // to just alias this one.
            try aro_args.append(arena, try std.fmt.allocPrint(arena, "-D___OpenBSD={d}", .{
                // Brilliantly, OpenBSD defines this macro to the year and month of the release, so we need to
                // maintain a manual mapping here whenever we update the headers.
                202510,
            }));
            // We can't avoid pollution for this one...
            try aro_args.append(arena, try std.fmt.allocPrint(arena, "-DOpenBSD{d}_{d}", .{
                min_ver.major,
                min_ver.minor,
            }));
        }
    }

    if (link_libcpp) {
        try aro_args.ensureUnusedCapacity(arena, 4);
        aro_args.appendAssumeCapacity("-isystem");
        aro_args.appendAssumeCapacity(try Io.Dir.path.join(arena, &.{ zig_lib_path, "libcxx", "include" }));

        aro_args.appendAssumeCapacity("-isystem");
        aro_args.appendAssumeCapacity(try Io.Dir.path.join(arena, &.{ zig_lib_path, "libcxxabi", "include" }));

        //try libcxx.addCxxArgs(comp, arena, aro_args);
    }

    const is_native_os = target_query.isNativeOs();
    const is_native_abi = target_query.isNativeAbi();
    const libc_installation = if (libc_paths_file) |libc_file|
        std.zig.LibCInstallation.parse(arena, io, libc_file, &target) catch |err| fatal("failed to parse '{s}': {t}", .{ libc_file, err })
    else
        null;

    const libc_dirs = std.zig.LibCDirs.detect(
        arena,
        io,
        .initCwd(zig_lib_path),
        &target,
        is_native_abi,
        link_libc,
        if (libc_installation) |l| &l else null,
        environ_map,
    ) catch |err| fatal("failed detecting libc: {t}", .{err});

    // Supplement Aro builtin headers with Clang builtin headers
    try aro_args.appendSlice(arena, &.{ "-idirafter", try Io.Dir.path.join(arena, &.{ zig_lib_path, "include" }) });

    try aro_args.ensureUnusedCapacity(arena, libc_dirs.libc_include_dir_list.len * 2);
    for (libc_dirs.libc_include_dir_list) |include_dir| {
        aro_args.appendAssumeCapacity("-isystem");
        aro_args.appendAssumeCapacity(include_dir);
    }

    const native_paths: ?std.zig.system.NativePaths = if (is_native_os and is_native_abi) p: {
        const native_paths = std.zig.system.NativePaths.detect(arena, io, &target, environ_map) catch |err|
            fatal("unable to detect native system paths: {t}", .{err});
        for (native_paths.warnings.items) |warning| {
            std.log.warn("{s}", .{warning});
        }
        break :p native_paths;
    } else null;

    if (native_paths) |np| {
        try aro_args.ensureUnusedCapacity(arena, np.include_dirs.items.len * 2);
        for (np.include_dirs.items) |include_path| {
            aro_args.appendAssumeCapacity("-isystem");
            aro_args.appendAssumeCapacity(include_path);
        }
    }

    if (link_libunwind) {
        try aro_args.ensureUnusedCapacity(arena, 2);
        aro_args.appendAssumeCapacity("-isystem");
        aro_args.appendAssumeCapacity(try Io.Dir.path.join(arena, &.{ zig_lib_path, "libunwind", "include" }));
    }

    try aro_args.ensureUnusedCapacity(arena, libc_dirs.libc_framework_dir_list.len * 2);
    for (libc_dirs.libc_framework_dir_list) |framework_dir| {
        try aro_args.appendSlice(arena, &.{ "-iframework", framework_dir });
    }

    if (native_paths) |np| {
        try aro_args.ensureUnusedCapacity(arena, np.framework_dirs.items.len * 2);
        for (np.framework_dirs.items) |framework_dir| {
            try aro_args.appendSlice(arena, &.{ "-F", framework_dir });
        }
    }

    var opt_pc: ?PkgConfig = null;
    if (any_want_pkg_conf or any_force_pkg_conf) {
        const pkg_config_exe = PkgConfig.exe(environ_map);
        if (process.run(arena, io, .{
            .argv = &.{ pkg_config_exe, "--list-all" },
            .environ_map = environ_map,
        })) |result| {
            if (result.term.success()) {
                opt_pc = try PkgConfig.init(arena, result.stdout, null);
            } else if (any_force_pkg_conf) {
                fatal("{s} {f}", .{ pkg_config_exe, result.term });
            }
        } else |err| {
            if (any_force_pkg_conf) fatal("{s}: failed running --list-all: {t}", .{ pkg_config_exe, err });
        }
    }

    if (opt_pc) |pc| {
        var group: Io.Group = .init;
        defer group.cancel(io);

        for (system_libs.items) |*system_lib| switch (system_lib.options.use_pkg_config) {
            .no => continue,
            .yes, .force => group.async(io, runPkgConfig, .{ arena, io, environ_map, &pc, system_lib }),
        };

        try group.await(io);
    }

    for (system_libs.items) |*system_lib| {
        if (system_lib.flags) |flags| {
            try aro_args.ensureUnusedCapacity(arena, flags.len + 1);
            aro_args.appendSliceAssumeCapacity(flags);
        }
    }

    const user_macros = macros: {
        var macro_buf: std.ArrayList(u8) = .empty;
        defer macro_buf.deinit(gpa);

        var discard_buf: [256]u8 = undefined;
        var discarding: Io.Writer.Discarding = .init(&discard_buf);
        assert(!try d.parseArgs(&discarding.writer, &macro_buf, aro_args.items));
        if (macro_buf.items.len > std.math.maxInt(u32)) {
            return d.fatal("user provided macro source exceeded max size", .{});
        }

        const content = try macro_buf.toOwnedSliceSentinel(gpa, 0);
        errdefer gpa.free(content);

        break :macros try d.comp.addSourceFromOwnedBuffer("<command line>", content, .user);
    };

    if (d.inputs.items.len != 1) {
        return d.fatal("expected exactly one input file", .{});
    }
    const source = d.inputs.items[0];

    tc.discover() catch |er| switch (er) {
        error.OutOfMemory => return error.OutOfMemory,
        error.TooManyMultilibs => return d.fatal("found more than one multilib with the same priority", .{}),
    };
    try tc.defineSystemIncludes();
    try d.comp.initSearchPath(d.includes.items, d.verbose_search_path);

    const builtin_macros = d.comp.generateBuiltinMacros(d.system_defines) catch |err| switch (err) {
        error.FileTooBig => return d.fatal("builtin macro source exceeded max size", .{}),
        else => |e| return e,
    };

    var pp = try aro.Preprocessor.init(d.comp, .{
        .base_file = source.id,
    });
    defer pp.deinit();

    var name_buf: [std.fs.max_name_bytes]u8 = undefined;
    var opt_dep_file = try d.initDepFile(source, &name_buf);
    defer if (opt_dep_file) |*dep_file| dep_file.deinit(d.comp.gpa);

    if (opt_dep_file) |*dep_file| pp.dep_file = dep_file;

    try pp.preprocessSources(.{
        .main = source,
        .builtin = builtin_macros,
        .command_line = user_macros,
        .imacros = d.imacros.items,
        .implicit_includes = d.implicit_includes.items,
    });

    var c_tree = try pp.parse();
    defer c_tree.deinit();

    if (d.diagnostics.errors != 0) process.exit(1);

    var out_buf: [4096]u8 = undefined;
    if (opt_dep_file) |dep_file| {
        const dep_file_name = try d.getDepFileName(source, out_buf[0..std.fs.max_name_bytes]);

        const file = if (dep_file_name) |path|
            d.comp.cwd.createFile(io, path, .{}) catch |er|
                return d.fatal("unable to create dependency file '{s}': {s}", .{ path, aro.Driver.errorDescription(er) })
        else
            Io.File.stdout();
        defer if (dep_file_name != null) file.close(io);

        var file_writer = file.writer(io, &out_buf);
        dep_file.write(&file_writer.interface) catch
            return d.fatal("unable to write dependency file: {s}", .{aro.Driver.errorDescription(file_writer.err.?)});
    }

    const rendered_zig = try Translator.translate(.{
        .gpa = gpa,
        .comp = d.comp,
        .pp = &pp,
        .tree = &c_tree,
        .module_libs = module_libs,
        .pub_static = pub_static,
        .func_bodies = func_bodies,
        .keep_macro_literals = keep_macro_literals,
        .default_init = default_init,
        .strict_flex_arrays = strict_flex_arrays,
    });
    defer gpa.free(rendered_zig);

    var close_out_file = false;
    var out_file_path: []const u8 = "<stdout>";
    var out_file: Io.File = .stdout();
    defer if (close_out_file) out_file.close(io);

    if (d.output_name) |path| blk: {
        if (std.mem.eql(u8, path, "-")) break :blk;
        if (std.fs.path.dirname(path)) |dirname| {
            Io.Dir.cwd().createDirPath(io, dirname) catch |err|
                return d.fatal("failed to create path to '{s}': {s}", .{ path, aro.Driver.errorDescription(err) });
        }
        out_file = Io.Dir.cwd().createFile(io, path, .{}) catch |err| {
            return d.fatal("failed to create output file '{s}': {s}", .{ path, aro.Driver.errorDescription(err) });
        };
        close_out_file = true;
        out_file_path = path;
    }

    var out_writer = out_file.writer(io, &out_buf);
    out_writer.interface.writeAll(rendered_zig) catch {};
    out_writer.interface.flush() catch {};
    if (out_writer.err) |write_err|
        return d.fatal("failed to write result to '{s}': {s}", .{ out_file_path, aro.Driver.errorDescription(write_err) });

    if (!module_libs) {
        const dest_path = if (d.output_name) |path| std.fs.path.dirname(path) else null;
        installLibs(d, dest_path) catch |err|
            return d.fatal("failed to install library files: {s}", .{aro.Driver.errorDescription(err)});
    }

    return process.cleanExit(io);
}

fn installLibs(d: *aro.Driver, dest_path: ?[]const u8) !void {
    const gpa = d.comp.gpa;
    const io = d.comp.io;
    const cwd = Io.Dir.cwd();

    const self_exe_path = try process.executableDirPathAlloc(io, gpa);
    defer gpa.free(self_exe_path);

    var cur_dir: []const u8 = self_exe_path;
    while (std.fs.path.dirname(cur_dir)) |dirname| : (cur_dir = dirname) {
        var base_dir = cwd.openDir(io, dirname, .{}) catch continue;
        defer base_dir.close(io);

        var lib_dir = base_dir.openDir(io, "lib", .{}) catch continue;
        defer lib_dir.close(io);

        lib_dir.access(io, "c_builtins.zig", .{}) catch continue;

        {
            const install_path = try std.fs.path.join(gpa, &.{ dest_path orelse "", "c_builtins.zig" });
            defer gpa.free(install_path);
            try lib_dir.copyFile("c_builtins.zig", cwd, install_path, io, .{});
        }
        {
            const install_path = try std.fs.path.join(gpa, &.{ dest_path orelse "", "helpers.zig" });
            defer gpa.free(install_path);
            try lib_dir.copyFile("helpers.zig", cwd, install_path, io, .{});
        }
        return;
    }
    return error.FileNotFound;
}

comptime {
    if (@import("builtin").is_test) {
        _ = Translator;
        _ = @import("helpers.zig");
        _ = @import("PatternList.zig");
    }
}

const SystemLib = struct {
    name: []const u8,
    options: std.Build.Module.LinkSystemLibraryOptions,
    flags: ?[]const []const u8 = null,
};

fn runPkgConfig(
    arena: Allocator,
    io: Io,
    environ_map: *const process.Environ.Map,
    pc: *const PkgConfig,
    system_lib: *SystemLib,
) void {
    const force = switch (system_lib.options.use_pkg_config) {
        .no => unreachable,
        .yes => false,
        .force => true,
    };

    const lib_name = system_lib.name;
    const pkg_config_exe = std.zig.PkgConfig.exe(environ_map);
    const found_index = pc.find(lib_name) orelse {
        if (force) fatal("{s}: package not found: {s}", .{ pkg_config_exe, lib_name });
        return;
    };
    const pkg = pc.all[found_index];

    const result = process.run(arena, io, .{
        .argv = &.{ pkg_config_exe, pkg.name, "--cflags", "--libs" },
        .environ_map = environ_map,
    }) catch |err| {
        if (force) fatal("failed running {s}: {t}", .{ pkg_config_exe, err });
        return;
    };
    if (!result.term.success()) {
        if (result.stderr.len != 0) std.log.err("{s}: {s}", .{ pkg_config_exe, result.stderr });
        if (force) fatal("{s} {f}", .{ pkg_config_exe, result.term });
        return;
    }

    system_lib.flags = ret: {
        var parsed: std.ArrayList([]const u8) = .empty;
        var arg_it = mem.tokenizeAny(u8, result.stdout, " \r\n\t");

        while (arg_it.next()) |arg| {
            parsed.append(arena, arg) catch |err| break :ret err;
        }

        parsed.shrinkToLen(arena) catch |err| break :ret err;
        break :ret parsed.toOwnedSliceAssert();
    } catch |err| switch (err) {
        error.OutOfMemory => fatal("out of memory parsing pkg-config output", .{}),
    };
}
