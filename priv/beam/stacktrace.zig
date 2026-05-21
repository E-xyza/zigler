const std = @import("std");
const builtin = @import("builtin");
const beam = @import("beam.zig");

const SelfInfo = std.debug.SelfInfo;
const Symbol = std.debug.Symbol;

var self_debug_info: ?SelfInfo = null;

var debug_info_allocator: ?std.mem.Allocator = null;
var debug_info_arena_allocator: std.heap.ArenaAllocator = undefined;

// ============================================================================
// Windows vendored implementation
// ============================================================================
//
// The standard library's Windows debug info loading has two issues in NIF context:
//
// 1. For relative PDB paths, it uses std.process.executableDirPathAlloc which
//    returns the BEAM VM's directory, not the NIF DLL's directory. We fix this
//    by using module.entry.FullDllName to get the DLL's own directory.
//
// 2. The file opening through the Io vtable was hanging (fixed in beam/io.zig
//    with dirOpenFileWindows override).

const vendored_windows = if (builtin.os.tag == .windows) struct {
    const windows = std.os.windows;
    const LDR = windows.LDR;
    const Allocator = std.mem.Allocator;
    const Io = std.Io;
    const coff = std.coff;
    const fs = std.fs;
    const Dwarf = std.debug.Dwarf;
    const Pdb = std.debug.Pdb;
    const Error = std.debug.SelfInfoError;
    const native_endian = builtin.target.cpu.arch.endian();

    const Module = struct {
        entry: *const LDR.DATA_TABLE_ENTRY,
        name: ?[]const u8,
        base_address: usize,
        di: ?Error!DebugInfo,

        const DebugInfo = struct {
            arena: std.heap.ArenaAllocator.State,
            coff_image_base: u64,
            mapped_file: ?MappedFile,
            dwarf: ?Dwarf,
            pdb: ?Pdb,
            coff_section_headers: []align(1) const coff.SectionHeader,

            const MappedFile = struct {
                file: Io.File,
                section_handle: windows.HANDLE,
                section_view: []const u8,

                fn deinit(mf: *const MappedFile, io: Io) void {
                    const process_handle = windows.GetCurrentProcess();
                    switch (windows.ntdll.NtUnmapViewOfSection(
                        process_handle,
                        @constCast(mf.section_view.ptr),
                    )) {
                        .SUCCESS => {},
                        else => |status| windows.unexpectedStatus(status) catch {},
                    }
                    windows.CloseHandle(mf.section_handle);
                    mf.file.close(io);
                }
            };

            fn deinit(di: *DebugInfo, gpa: Allocator, io: Io) void {
                if (di.dwarf) |*dwarf| dwarf.deinit(gpa);
                if (di.pdb) |*pdb| {
                    pdb.file_reader.file.close(io);
                    pdb.deinit();
                }
                if (di.mapped_file) |*mf| mf.deinit(io);

                var arena = di.arena.promote(gpa);
                arena.deinit();
            }

            fn getSymbols(
                di: *DebugInfo,
                symbol_allocator: Allocator,
                text_arena: Allocator,
                vaddr: usize,
                resolve_inline_callers: bool,
                symbols: *std.ArrayList(std.debug.Symbol),
            ) Error!void {
                pdb: {
                    const pdb = &(di.pdb orelse break :pdb);
                    var coff_section: *align(1) const coff.SectionHeader = undefined;
                    const mod_index = for (pdb.sect_contribs) |sect_contrib| {
                        if (sect_contrib.section > di.coff_section_headers.len) continue;
                        coff_section = &di.coff_section_headers[sect_contrib.section - 1];

                        const vaddr_start = coff_section.virtual_address + sect_contrib.offset;
                        const vaddr_end = vaddr_start + sect_contrib.size;
                        if (vaddr >= vaddr_start and vaddr < vaddr_end) {
                            break sect_contrib.module_index;
                        }
                    } else {
                        break :pdb;
                    };

                    const module = pdb.getModule(mod_index) catch |err| switch (err) {
                        error.InvalidDebugInfo,
                        error.MissingDebugInfo,
                        error.OutOfMemory,
                        => |e| return e,
                        error.ReadFailed,
                        error.EndOfStream,
                        => return error.InvalidDebugInfo,
                    } orelse {
                        return error.InvalidDebugInfo;
                    };

                    const addr = vaddr - coff_section.virtual_address;
                    const maybe_proc = pdb.getProcSym(module, addr);
                    const compile_unit_name = fs.path.basename(module.obj_file_name);
                    const symbols_top = symbols.items.len;

                    if (maybe_proc) |proc| {
                        const offset_in_func = addr - proc.code_offset;
                        var last_inlinee: ?u32 = null;
                        var iter = pdb.getInlinees(module, proc);
                        while (iter.next(module)) |inline_site| {
                            if (inline_site.inlinee == last_inlinee) continue;

                            for (pdb.getInlineeSourceLines(module, inline_site.inlinee)) |inlinee_src_line| {
                                const maybe_loc = pdb.getInlineSiteSourceLocation(
                                    text_arena,
                                    module,
                                    inline_site,
                                    inlinee_src_line.info,
                                    offset_in_func,
                                ) catch continue;
                                const loc = maybe_loc orelse continue;

                                if (!resolve_inline_callers and inline_site.inlinee != last_inlinee) {
                                    symbols.items.len = symbols_top;
                                }

                                const name = if (resolve_inline_callers)
                                    pdb.findInlineeName(inline_site.inlinee)
                                else
                                    null;

                                try symbols.append(symbol_allocator, .{
                                    .name = name,
                                    .compile_unit_name = compile_unit_name,
                                    .source_location = loc,
                                });

                                last_inlinee = inline_site.inlinee;
                            }
                        }

                        if (resolve_inline_callers) {
                            std.mem.reverse(std.debug.Symbol, symbols.items);
                        } else if (last_inlinee) |inlinee| {
                            const name = pdb.findInlineeName(inlinee);
                            for (symbols.items) |*symbol| symbol.name = name;
                        }
                    }

                    if (resolve_inline_callers or symbols.items.len == 0) {
                        try symbols.append(symbol_allocator, .{
                            .name = if (maybe_proc) |proc| pdb.getSymbolName(proc) else null,
                            .compile_unit_name = compile_unit_name,
                            .source_location = pdb.getLineNumberInfo(text_arena, module, addr) catch null,
                        });
                    }

                    return;
                }

                dwarf: {
                    const dwarf = &(di.dwarf orelse break :dwarf);
                    const addr = vaddr + di.coff_image_base;
                    return dwarf.getSymbols(symbol_allocator, text_arena, native_endian, addr, resolve_inline_callers, symbols);
                }

                return error.MissingDebugInfo;
            }
        };

        fn getDebugInfo(module: *Module, gpa: Allocator, io: Io) Error!*DebugInfo {
            if (module.di == null) {
                module.di = loadDebugInfo(module, gpa, io);
            }
            return if (module.di.?) |*di| di else |err| err;
        }

        fn loadDebugInfo(module: *const Module, gpa: Allocator, io: Io) Error!DebugInfo {
            const mapped_ptr: [*]const u8 = @ptrCast(module.entry.DllBase);
            const mapped = mapped_ptr[0..module.entry.SizeOfImage];

            var coff_obj = coff.Coff.init(mapped, true) catch return error.InvalidDebugInfo;

            var arena_instance: std.heap.ArenaAllocator = .init(gpa);
            errdefer arena_instance.deinit();
            const arena = arena_instance.allocator();

            const mapped_file: ?DebugInfo.MappedFile = mapped: {
                if (!coff_obj.strtabRequired()) {
                    break :mapped null;
                }

                var path_buffer: [4 + windows.PATH_MAX_WIDE]u16 = undefined;
                path_buffer[0..4].* = .{ '\\', '?', '?', '\\' };
                const path_slice = module.entry.FullDllName.slice();
                @memcpy(path_buffer[4..][0..path_slice.len], path_slice);

                const coff_file = Io.Threaded.dirOpenFileWtf16(null, path_buffer[0 .. 4 + path_slice.len], .{}) catch |err| {
                    return switch (err) {
                        error.Canceled, error.Unexpected => |e| e,
                        error.FileNotFound => error.MissingDebugInfo,
                        error.FileTooBig, error.IsDir, error.NotDir, error.SymLinkLoop, error.NameTooLong, error.BadPathName => error.InvalidDebugInfo,
                        else => error.ReadFailed,
                    };
                };
                errdefer coff_file.close(io);

                var section_handle: windows.HANDLE = undefined;
                const create_section_rc = windows.ntdll.NtCreateSection(
                    &section_handle,
                    .{ .SPECIFIC = .{ .SECTION = .{ .QUERY = true, .MAP_READ = true } }, .STANDARD = .{ .RIGHTS = .REQUIRED } },
                    null,
                    null,
                    .{ .READONLY = true },
                    .{ .COMMIT = true },
                    coff_file.handle,
                );
                if (create_section_rc != .SUCCESS) {
                    return error.MissingDebugInfo;
                }
                errdefer windows.CloseHandle(section_handle);

                var coff_len: usize = 0;
                var section_view_ptr: ?[*]const u8 = null;
                const map_section_rc = windows.ntdll.NtMapViewOfSection(
                    section_handle,
                    windows.GetCurrentProcess(),
                    @ptrCast(&section_view_ptr),
                    null,
                    0,
                    null,
                    &coff_len,
                    .Unmap,
                    .{},
                    .{ .READONLY = true },
                );
                if (map_section_rc != .SUCCESS) {
                    return error.MissingDebugInfo;
                }

                const section_view = section_view_ptr.?[0..coff_len];
                coff_obj = coff.Coff.init(section_view, false) catch return error.InvalidDebugInfo;
                break :mapped .{ .file = coff_file, .section_handle = section_handle, .section_view = section_view };
            };
            errdefer if (mapped_file) |*mf| mf.deinit(io);

            const coff_image_base = coff_obj.getImageBase();

            var opt_dwarf: ?Dwarf = dwarf: {
                if (coff_obj.getSectionByName(".debug_info") == null) {
                    break :dwarf null;
                }
                var sections: Dwarf.SectionArray = undefined;
                inline for (@typeInfo(Dwarf.Section.Id).@"enum".fields, 0..) |section, i| {
                    sections[i] = if (coff_obj.getSectionByName("." ++ section.name)) |section_header| .{
                        .data = try coff_obj.getSectionDataAlloc(section_header, arena),
                        .owned = false,
                    } else null;
                }
                break :dwarf .{ .sections = sections };
            };
            errdefer if (opt_dwarf) |*dwarf| dwarf.deinit(gpa);

            if (opt_dwarf) |*dwarf| {
                dwarf.open(gpa, native_endian) catch |err| {
                    return switch (err) {
                        error.Overflow, error.EndOfStream, error.StreamTooLong, error.ReadFailed => error.InvalidDebugInfo,
                        error.InvalidDebugInfo, error.MissingDebugInfo, error.OutOfMemory => |e| e,
                    };
                };
            }

            var opt_pdb: ?Pdb = pdb: {
                const path = coff_obj.getPdbPath() catch {
                    return error.InvalidDebugInfo;
                } orelse {
                    break :pdb null;
                };

                // Key fix: For relative PDB paths, use the DLL's directory instead of
                // the executable directory (which would be the BEAM VM's directory).
                const pdb_file_open_result = if (fs.path.isAbsolute(path)) res: {
                    break :res Io.Dir.cwd().openFile(io, path, .{});
                } else res: {
                    const dll_name_wide = module.entry.FullDllName.slice();
                    var dll_path_buf: [windows.PATH_MAX_WIDE]u8 = undefined;
                    const dll_path_len = std.unicode.wtf16LeToWtf8(&dll_path_buf, dll_name_wide);
                    const dll_path = dll_path_buf[0..dll_path_len];

                    const dll_dir = fs.path.dirname(dll_path) orelse {
                        break :res error.InvalidDebugInfo;
                    };

                    const abs_path = fs.path.join(gpa, &.{ dll_dir, path }) catch |err| {
                        break :res err;
                    };
                    defer gpa.free(abs_path);
                    break :res Io.Dir.cwd().openFile(io, abs_path, .{});
                };

                const pdb_file = pdb_file_open_result catch |err| {
                    return switch (err) {
                        error.FileNotFound, error.IsDir => {
                            break :pdb null;
                        },
                        else => error.ReadFailed,
                    };
                };
                errdefer pdb_file.close(io);

                const pdb_reader = try arena.create(Io.File.Reader);
                pdb_reader.* = pdb_file.reader(io, try arena.alloc(u8, 4096));

                var pdb = Pdb.init(gpa, pdb_reader) catch |err| {
                    return switch (err) {
                        error.OutOfMemory, error.ReadFailed, error.Unexpected => |e| e,
                        else => error.InvalidDebugInfo,
                    };
                };
                errdefer pdb.deinit();

                pdb.parseInfoStream() catch |err| {
                    return switch (err) {
                        error.UnknownPDBVersion => error.UnsupportedDebugInfo,
                        error.EndOfStream => error.InvalidDebugInfo,
                        error.InvalidDebugInfo, error.MissingDebugInfo, error.OutOfMemory, error.ReadFailed => |e| e,
                    };
                };

                pdb.parseDbiStream() catch |err| {
                    return switch (err) {
                        error.UnknownPDBVersion => error.UnsupportedDebugInfo,
                        error.EndOfStream, error.EOF, error.StreamTooLong, error.WriteFailed => error.InvalidDebugInfo,
                        error.InvalidDebugInfo, error.OutOfMemory, error.ReadFailed => |e| e,
                    };
                };

                pdb.parseIpiStream() catch |err| {
                    return switch (err) {
                        error.UnknownPDBVersion => error.UnsupportedDebugInfo,
                        error.EndOfStream => error.InvalidDebugInfo,
                        error.OutOfMemory, error.ReadFailed => |e| e,
                    };
                };

                if (!std.mem.eql(u8, &coff_obj.guid, &pdb.guid) or coff_obj.age != pdb.age) {
                    return error.InvalidDebugInfo;
                }

                break :pdb pdb;
            };
            errdefer if (opt_pdb) |*pdb| {
                pdb.file_reader.file.close(io);
                pdb.deinit();
            };

            return .{
                .arena = arena_instance.state,
                .coff_image_base = coff_image_base,
                .dwarf = opt_dwarf,
                .pdb = opt_pdb,
                .mapped_file = mapped_file,
                .coff_section_headers = coff_obj.getSectionHeaders(),
            };
        }
    };

    // Module cache
    var modules: [16]?Module = .{null} ** 16;

    fn findModule(address: usize) Error!*Module {
        // Check cache
        for (&modules) |*mod| {
            if (mod.*) |*m| {
                if (address >= m.base_address and
                    address < m.base_address + m.entry.SizeOfImage)
                {
                    return m;
                }
            }
        }

        // Not cached, lookup via LdrFindEntryForAddress
        var entry: *LDR.DATA_TABLE_ENTRY = undefined;
        if (windows.ntdll.LdrFindEntryForAddress(@ptrFromInt(address), &entry) != .SUCCESS) {
            return error.MissingDebugInfo;
        }

        // Find empty cache slot
        const mod = for (&modules) |*m| {
            if (m.* == null) break m;
        } else &modules[0];

        mod.* = .{
            .entry = entry,
            .name = null,
            .base_address = @intFromPtr(entry.DllBase),
            .di = null,
        };

        return &mod.*.?;
    }

    fn getSymbolsVendored(
        io: Io,
        symbol_allocator: Allocator,
        text_arena: Allocator,
        address: usize,
        resolve_inline_callers: bool,
        symbols: *std.ArrayList(std.debug.Symbol),
    ) Error!void {
        const gpa = std.debug.getDebugInfoAllocator();
        const module = findModule(address) catch return;
        const di = module.getDebugInfo(gpa, io) catch return;
        di.getSymbols(
            symbol_allocator,
            text_arena,
            address - module.base_address,
            resolve_inline_callers,
            symbols,
        ) catch return;
    }
} else struct {
    fn getSymbolsVendored(
        io: std.Io,
        symbol_allocator: std.mem.Allocator,
        text_arena: std.mem.Allocator,
        address: usize,
        resolve_inline_callers: bool,
        symbols: *std.ArrayList(std.debug.Symbol),
    ) std.debug.SelfInfoError!void {
        _ = io;
        _ = symbol_allocator;
        _ = text_arena;
        _ = address;
        _ = resolve_inline_callers;
        _ = symbols;
    }
};

// ============================================================================
// Stacktrace conversion
// ============================================================================

fn getDebugInfoAllocator() std.mem.Allocator {
    if (debug_info_allocator) |a| return a;

    debug_info_arena_allocator = std.heap.ArenaAllocator.init(beam.allocator);
    const allocator = debug_info_arena_allocator.allocator();
    debug_info_allocator = allocator;
    return allocator;
}

fn getSelfInfo() *SelfInfo {
    if (self_debug_info) |*info| {
        return info;
    } else {
        self_debug_info = SelfInfo.init;
        return &self_debug_info.?;
    }
}

fn make_empty_trace_item(opts: anytype) beam.term {
    return beam.make(.{
        .source_location = null,
        .symbol_name = null,
        .compile_unit_name = null,
    }, opts);
}

fn make_trace_item(debug_info: *SelfInfo, io: std.Io, address: usize, opts: anytype) beam.term {
    // On Windows, just return empty trace items for now
    // Full symbol resolution is complex and can cause issues
    if (comptime builtin.os.tag == .windows) {
        return make_empty_trace_item(opts);
    }

    const allocator = getDebugInfoAllocator();

    var symbols: std.ArrayList(Symbol) = .empty;
    defer symbols.deinit(allocator);

    debug_info.getSymbols(io, allocator, allocator, address, false, &symbols) catch {
        return make_empty_trace_item(opts);
    };

    if (symbols.items.len == 0) {
        return make_empty_trace_item(opts);
    }

    const symbol = symbols.items[0];

    return beam.make(.{
        .source_location = symbol.source_location,
        .symbol_name = symbol.name,
        .compile_unit_name = symbol.compile_unit_name,
    }, opts);
}

pub fn to_term(stacktrace: *std.builtin.StackTrace, opts: anytype) beam.term {
    if (builtin.strip_debug_info) return beam.make_empty_list(opts);

    const debug_info = getSelfInfo();
    const io = beam.context.io;

    var frame_index: usize = 0;
    var frames_left: usize = @min(stacktrace.index, stacktrace.instruction_addresses.len);
    var stacktrace_term = beam.make_empty_list(opts);

    while (frames_left != 0) : ({
        frames_left -= 1;
        frame_index = (frame_index + 1) % stacktrace.instruction_addresses.len;
    }) {
        const return_address = stacktrace.instruction_addresses[frame_index];
        const new_trace_item = make_trace_item(debug_info, io, return_address -| 1, opts);
        stacktrace_term = beam.make_list_cell(new_trace_item, stacktrace_term, opts);
    }
    return stacktrace_term;
}
