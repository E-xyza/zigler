//! stubs erl_nif functions.  This is necessary so that the linker doesn't scream at you when
//! you are doing semantic analysis, which needs to be linked at them.

pub const is_sema = true;

// size zero types
pub const ERL_NIF_TERM = extern struct { __x__: c_int = 0 };
pub const ERL_NIF_MODULE = extern struct {};
pub const ErlNifTerm = ERL_NIF_TERM;
pub const ErlNifEnv = extern struct { __z__: c_int = 0 };
pub const ErlNifBinary = extern struct { data: [*c]u8, size: usize };
pub const ErlNifResourceType = extern struct {};
pub const ErlNifBinaryToTerm = c_int;
pub const ErlNifMonitor = extern struct {};
pub const ErlNifPid = extern struct {};
pub const ErlNifPort = extern struct {};
pub const ErlNifCond = extern struct {};
pub const ErlNifMutex = extern struct {};
pub const ErlNifTime = extern struct {};
pub const ErlNifTid = extern struct {};
pub const ErlNifThreadOpts = extern struct {};
pub const ErlNifIOVec = extern struct {};
pub const ErlNifIOQueue = extern struct {};
pub const ErlNifIOQueueOpts = extern struct {};
pub const ErlNifMapIterator = extern struct {};
pub const ErlNifMapIteratorEntry = extern struct {};
pub const ErlNifResourceFlags = c_int;
pub const ErlNifSelectFlags = extern struct {};
pub const ErlNifEvent = extern struct { v: c_int = 0 };
pub const ErlNifRWLock = extern struct {};
pub const ErlNifSysInfo = extern struct {};
pub const ErlNifTSDKey = extern struct {};
pub const SysIOVec = extern struct {};

// dependent function types.
pub const ErlNifResourceDtor = fn (env, ?*anyopaque) callconv(.c) void;
pub const ErlNifResourceDown = fn (env, ?*anyopaque, pid, mon) callconv(.c) void;
pub const ErlNifResourceStop = fn (env, ?*anyopaque, event, c_int) callconv(.c) void;
pub const ErlNifResourceDynCall = fn (env, ?*anyopaque, ?*anyopaque) callconv(.c) void;

const dtor = ErlNifResourceDtor;
const down = ErlNifResourceDown;
const stop = ErlNifResourceStop;
const dyncall = ErlNifResourceDynCall;

pub const ErlNifResourceTypeInit = extern struct {
    dtor: ?*const dtor,
    stop: ?*const stop,
    down: ?*const down,
    dyncall: ?*const dyncall,
    members: c_int,
};

// enum types.  These types are enums in the BEAM, we're going to spoof them
// as integers.
pub const ErlNifTimeUnit = u64;
pub const ErlNifCharEncoding = u64;
pub const ErlNifHash = u64;
pub const ErlNifTermType = u64;
pub const ErlNifUniqueInteger = u64;

// convenience abbreviations for this document only.
// three letters is for types that are always used as pointers.
const env = ?*ErlNifEnv;
const bin = ?*ErlNifBinary;
const res = ?*ErlNifResourceType;
const ini = ?*const ErlNifResourceTypeInit;
const mon = ?*ErlNifMonitor;
const pid = ?*ErlNifPid;
const prt = ?*ErlNifPort;
const cnd = ?*ErlNifCond;
const mtx = ?*ErlNifMutex;
const ioq = ?*ErlNifIOQueue;
const iov = ?*ErlNifIOVec;
const mpi = ?*ErlNifMapIterator;
const lck = ?*ErlNifRWLock;
const inf = ?*ErlNifSysInfo;
const top = ?*ErlNifThreadOpts;
const trm = ?*ERL_NIF_TERM;
const siv = ?*SysIOVec;
// more than four characters is not a pointer type.
const term = ERL_NIF_TERM;
const modl = ERL_NIF_MODULE;
const cenc = ErlNifCharEncoding;
const time = ErlNifTime;
const timeu = ErlNifTimeUnit;
const tid_ = ErlNifTid;
const hash = ErlNifHash;
const ioqo = ErlNifIOQueueOpts;
const mpie = ErlNifMapIteratorEntry;
const unqi = ErlNifUniqueInteger;
const rflags = ErlNifResourceFlags;
const sflags = ErlNifSelectFlags;
const event = ErlNifEvent;
const ttyp = ErlNifTermType;
const tsdk = ErlNifTSDKey;

const reentry = fn (env, c_int, [*c]const term) callconv(.c) term;

pub fn enif_alloc(_: usize) ?*anyopaque {
    return null;
}
pub fn enif_alloc_binary(_: usize, _: bin) c_int {
    return 0;
}
pub fn enif_alloc_env() env {
    return null;
}
pub fn enif_alloc_resource(_: res, _: c_uint) ?*anyopaque {
    return null;
}
pub fn enif_binary_to_term(_: env, _: [*c]u8, _: usize, _: *term, _: ErlNifBinaryToTerm) usize {
    return 0;
}
pub fn enif_clear_env(_: env) void {}
pub fn enif_compare(_: term, _: term) c_int {
    return 0;
}
pub fn enif_compare_monitors(_: mon, _: mon) c_int {
    return 0;
}
pub fn enif_compare_pids(_: pid, _: pid) c_int {
    return 0;
}
pub fn enif_cond_broadcast(_: cnd) void {}
pub fn enif_cond_create(_: [*c]u8) cnd {
    return null;
}
pub fn enif_cond_destroy(_: cnd) void {}
pub fn enif_cond_name(_: cnd) [*c]u8 {}
pub fn enif_cond_signal(_: cnd) void {}
pub fn enif_cond_wait(_: cnd, _: mtx) void {}
pub fn enif_consume_timeslice(_: env, _: c_int) c_int {
    return 0;
}
pub fn enif_convert_time_unit(_: time, _: timeu, _: timeu) time {
    return .{};
}
pub fn enif_cpu_time(_: env) term {
    return .{};
}
pub fn enif_demonitor_process(_: env, _: ?*anyopaque, _: mon) c_int {
    return 0;
}
pub fn enif_dynamic_resource_call(_: env, _: modl, _: modl, _: term, _: ?*anyopaque) c_int {
    return 0;
}
pub fn enif_equal_tids(_: tid_, _: tid_) c_int {
    return 0;
}
// pub fn enif_fprintf <-- variadic function.  can't stub.
pub fn enif_free(_: ?*anyopaque) void {}
pub fn enif_free_env(_: env) void {}
pub fn enif_free_iovec(_: iov) void {}
pub fn enif_get_atom(_: env, _: term, _: [*c]u8, _: c_uint, _: cenc) c_int {
    return 0;
}
pub fn enif_get_atom_length(_: env, _: term, _: ?*c_uint, _: cenc) c_int {
    return 0;
}
pub fn enif_get_double(_: env, _: term, _: ?*f64) c_int {
    return 0;
}
pub fn enif_get_int(_: env, _: term, _: ?*c_int) c_int {
    return 0;
}
pub fn enif_get_int64(_: env, _: term, _: ?*i64) c_int {
    return 0;
}
pub fn enif_get_local_pid(_: env, _: term, _: pid) c_int {
    return 0;
}
pub fn enif_get_local_port(_: env, _: term, _: prt) c_int {
    return 0;
}
pub fn enif_get_list_cell(_: env, _: term, _: trm, _: trm) c_int {
    return 0;
}
pub fn enif_get_list_length(_: env, _: term, _: ?*c_uint) c_int {
    return 0;
}
pub fn enif_get_long(_: env, _: term, _: ?*c_long) c_int {
    return 0;
}
pub fn enif_get_map_size(_: env, _: term, _: ?*usize) c_int {
    return 0;
}
pub fn enif_get_map_value(_: env, _: term, _: term, _: trm) c_int {
    return 0;
}
pub fn enif_get_resource(_: env, _: term, _: res, _: ?*?*anyopaque) c_int {
    return 0;
}
pub fn enif_get_string(_: env, _: term, _: [*c]u8, _: c_uint, _: cenc) c_int {
    return 0;
}
pub fn enif_get_tuple(_: env, _: term, _: ?*c_int, _: ?*trm) c_int {
    return 0;
}
pub fn enif_get_uint(_: env, _: term, _: ?*c_uint) c_int {
    return 0;
}
pub fn enif_get_uint64(_: env, _: term, _: ?*u64) c_int {
    return 0;
}
pub fn enif_get_ulong(_: env, _: term, _: ?*c_ulong) c_int {
    return 0;
}
pub fn enif_getenv(_: [*c]const u8, _: [*c]u8, _: ?*usize) c_int {
    return 0;
}
pub fn enif_has_pending_exception(_: env, _: trm) c_int {
    return 0;
}
pub fn enif_hash(_: hash, _: term, _: u64) u64 {
    return 0;
}
pub fn enif_inspect_binary(_: env, _: term, _: bin) c_int {
    return 0;
}
pub fn enif_inspect_iolist_as_binary(_: env, _: term, _: bin) c_int {
    return 0;
}
pub fn enif_inspect_iovec(_: env, _: usize, _: term, _: trm, _: ?*iov) c_int {
    return 0;
}
pub fn enif_ioq_create(_: ioqo) ioq {
    return null;
}
pub fn enif_ioq_destroy(_: ioq) void {}
pub fn enif_ioq_deq(_: ioq, _: usize, _: ?*usize) c_int {
    return 0;
}
pub fn enif_ioq_enq_binary(_: ioq, _: bin, _: usize) c_int {
    return 0;
}
pub fn enif_ioq_enqv(_: ioq, _: iov, _: usize) c_int {
    return 0;
}
pub fn enif_ioq_peek(_: ioq, _: ?*c_int) siv {
    return 0;
}
pub fn enif_ioq_peek_head(_: env, _: ioq, _: ?*c_int, _: trm) siv {
    return 0;
}
pub fn enif_ioq_size(_: ioq) usize {
    return 0;
}
pub fn enif_is_atom(_: env, _: term) c_int {
    return 0;
}
pub fn enif_is_binary(_: env, _: term) c_int {
    return 0;
}
pub fn enif_is_current_process_alive(_: env) c_int {
    return 0;
}
pub fn enif_is_empty_list(_: env, _: term) c_int {
    return 0;
}
pub fn enif_is_exception(_: env, _: term) c_int {
    return 0;
}
pub fn enif_is_fun(_: env, _: term) c_int {
    return 0;
}
pub fn enif_is_identical(_: term, _: term) c_int {
    return 0;
}
pub fn enif_is_list(_: env, _: term) c_int {
    return 0;
}
pub fn enif_is_map(_: env, _: term) c_int {
    return 0;
}
pub fn enif_is_number(_: env, _: term) c_int {
    return 0;
}
pub fn enif_is_pid(_: env, _: term) c_int {
    return 0;
}
pub fn enif_is_pid_undefined(_: env, _: term) c_int {
    return 0;
}
pub fn enif_is_port(_: env, _: term) c_int {
    return 0;
}
pub fn enif_is_port_alive(_: env, _: prt) c_int {
    return 0;
}
pub fn enif_is_process_alive(_: env, _: pid) c_int {
    return 0;
}
pub fn enif_is_ref(_: env, _: term) c_int {
    return 0;
}
pub fn enif_is_tuple(_: env, _: term) c_int {
    return 0;
}
pub fn enif_keep_resource(_: ?*anyopaque) c_int {
    return 0;
}
pub fn enif_make_atom(_: env, _: [*c]const u8) term {
    return .{};
}
pub fn enif_make_atom_len(_: env, _: [*c]const u8, _: usize) term {
    return .{};
}
pub fn enif_make_badarg(_: env) term {
    return .{};
}
pub fn enif_make_binary(_: env, _: bin) term {
    return .{};
}
pub fn enif_make_copy(_: env, _: term) term {
    return .{};
}
pub fn enif_make_double(_: env, _: f64) term {
    return .{};
}
pub fn enif_make_existing_atom(_: env, _: [*c]const u8, _: trm, _: cenc) c_int {
    return 0;
}
pub fn enif_make_existing_atom_len(_: env, _: [*c]const u8, _: usize, _: trm, _: cenc) c_int {
    return 0;
}
pub fn enif_make_int(_: env, _: c_int) term {
    return .{};
}
pub fn enif_make_int64(_: env, _: u64) term {
    return .{};
}
// don't use the variadic list functions.
// pub fn enif_make_list(...)
// pub fn enif_make_list1(...)
// ...
// pub fn enif_make_list9(...)
pub fn enif_make_list_cell(_: env, _: term, _: term) term {
    return .{};
}
pub fn enif_make_list_from_array(_: env, _: [*c]const term, _: c_int) term {
    return .{};
}
pub fn enif_make_long(_: env, _: c_long) term {
    return .{};
}
pub fn enif_make_map_put(_: env, _: term, _: term, _: term, _: trm) c_int {
    return 0;
}
pub fn enif_make_map_remove(_: env, _: term, _: term, _: trm) c_int {
    return 0;
}
pub fn enif_make_map_update(_: env, _: term, _: term, _: term, _: trm) c_int {
    return 0;
}
pub fn enif_make_map_from_arrays(_: env, _: [*c]const term, _: [*c]const term, _: usize, _: trm) c_int {
    return 0;
}
pub fn enif_make_monitor_term(_: env, _: mon) term {
    return .{};
}
pub fn enif_make_new_binary(_: env, _: usize, _: trm) [*c]u8 {
    return null;
}
pub fn enif_make_new_map(_: env) term {
    return .{};
}
pub fn enif_make_pid(_: env, _: pid) term {
    return .{};
}
pub fn enif_make_ref(_: env) term {
    return .{};
}
pub fn enif_make_resource(_: env, _: ?*anyopaque) term {
    return .{};
}
pub fn enif_make_resource_binary(_: env, _: ?*anyopaque, _: ?*const anyopaque, _: usize) term {
    return .{};
}
pub fn enif_make_reverse_list(_: env, _: term, _: trm) c_int {
    return 0;
}
pub fn enif_make_string(_: env, _: [*c]const u8, _: cenc) term {
    return .{};
}
pub fn enif_make_string_len(_: env, _: [*c]const u8, _: usize, _: cenc) term {
    return .{};
}
pub fn enif_make_sub_binary(_: env, _: term, _: usize, _: usize) term {
    return .{};
}
// don't use the variadic tuple functions.
// pub fn enif_make_tuple(...)
// pub fn enif_make_tuple1(...)
// ...
// pub fn enif_make_tuple9(...)
pub fn enif_make_tuple_from_array(_: env, _: [*c]const term, _: c_uint) term {
    return .{};
}
pub fn enif_make_uint(_: env, _: c_uint) term {
    return .{};
}
pub fn enif_make_uint64(_: env, _: u64) term {
    return .{};
}
pub fn enif_make_ulong(_: env, _: c_ulong) term {
    return .{};
}
pub fn enif_make_unique_integer(_: env, _: unqi) term {
    return .{};
}
pub fn enif_map_iterator_create(_: env, _: term, _: mpi, _: mpie) c_int {
    return 0;
}
pub fn enif_map_iterator_destroy(_: env, _: mpi) void {}
pub fn enif_map_iterator_get_pair(_: env, _: mpi, _: trm, _: trm) c_int {
    return 0;
}
pub fn enif_map_iterator_is_head(_: env, _: mpi) c_int {
    return 0;
}
pub fn enif_map_iterator_is_tail(_: env, _: mpi) c_int {
    return 0;
}
pub fn enif_map_iterator_next(_: env, _: mpi) c_int {
    return 0;
}
pub fn enif_map_iterator_prev(_: env, _: mpi) c_int {
    return 0;
}
pub fn enif_monitor_process(_: env, _: ?*anyopaque, _: pid, _: mon) c_int {
    return 0;
}
pub fn enif_monotonic_time(_: timeu) time {
    return .{};
}
pub fn enif_mutex_create(_: [*c]u8) mtx {
    return null;
}
pub fn enif_mutex_destroy(_: mtx) void {}
pub fn enif_mutex_lock(_: mtx) void {}
pub fn enif_mutex_name(_: mtx) [*c]u8 {
    return null;
}
pub fn enif_mutex_trylock(_: mtx) c_int {
    return 0;
}
pub fn enif_mutex_unlock(_: mtx) void {}
// next function is deprecated
// pub fn enif_now_time(...)
pub fn enif_open_resource_type(_: env, _: [*c]const u8, _: [*c]const u8, _: dtor, _: rflags, _: ?*rflags) res {
    return null;
}
pub fn enif_open_resource_type_x(_: env, _: [*c]const u8, _: ini, _: rflags, _: ?*rflags) res {
    return null;
}
pub fn enif_init_resource_type(_: env, _: [*c]const u8, _: ini, _: rflags, _: ?*rflags) res {
    return null;
}
pub fn enif_port_command(_: env, _: prt, _: env, _: term) c_int {
    return .{};
}
pub fn enif_priv_data(_: env) ?*anyopaque {
    return null;
}
pub fn enif_raise_exception(_: env, _: term) term {
    return .{};
}
pub fn enif_realloc(_: ?*anyopaque, _: usize) ?*anyopaque {
    return null;
}
pub fn enif_realloc_binary(_: bin, _: usize) c_int {
    return null;
}
pub fn enif_release_binary(_: bin) void {}
pub fn enif_release_resource(_: ?*anyopaque) void {}
pub fn enif_rwlock_create(_: [*c]u8) lck {
    return null;
}
pub fn enif_rwlock_destroy(_: lck) void {}
pub fn enif_rwlock_name(_: lck) [*c]u8 {}
pub fn enif_rwlock_rlock(_: lck) void {}
pub fn enif_rwlock_runlock(_: lck) void {}
pub fn enif_rwlock_rwunlock(_: lck) void {}
pub fn enif_rwlock_tryrlock(_: lck) c_int {
    return 0;
}
pub fn enif_rwlock_tryrwlock(_: lck) c_int {
    return 0;
}
pub fn enif_schedule_nif(_: env, _: [*c]const u8, _: c_int, _: reentry, _: c_int, _: [*c]const term) term {
    return .{};
}
pub fn enif_select(_: env, _: event, _: sflags, _: ?*anyopaque, _: pid, _: term) c_int {
    return 0;
}
pub fn enif_select_read(_: env, _: event, _: ?*anyopaque, _: pid, _: term, _: env) c_int {
    return 0;
}
pub fn enif_select_write(_: env, _: event, _: ?*anyopaque, _: pid, _: term, _: env) c_int {
    return 0;
}
pub fn enif_self(_: env, _: pid) pid {
    return null;
}
pub fn enif_send(_: env, _: pid, _: env, _: term) c_int {
    return 0;
}
pub fn enif_set_pid_undefined(_: pid) void {}
pub fn enif_sizeof_resource(_: ?*anyopaque) c_uint {
    return 0;
}
// not usable since it's variadic
// pub fn enif_snprintf(...)
pub fn enif_system_info(_: inf, _: usize) void {}
pub fn enif_term_to_binary(_: env, _: term, _: bin) c_int {
    return 0;
}
pub fn enif_term_type(_: env, _: term) ttyp {
    return 0;
}
pub fn enif_thread_create(_: [*c]u8, _: ?*tid_, _: *const fn (?*anyopaque) callconv(.c) ?*anyopaque, _: ?*anyopaque, _: top) c_int {
    return 0;
}
pub fn enif_thread_exit(_: ?*anyopaque) void {}
pub fn enif_thread_join(_: tid_, _: ?*?*anyopaque) c_int {
    return 0;
}
pub fn enif_thread_name(_: tid_) [*c]u8 {
    return null;
}
pub fn enif_therad_opts_create(_: [*c]u8) top {
    return null;
}
pub fn enif_thread_opts_destroy(_: top) void {}
pub fn enif_thread_self() tid_ {
    return .{};
}
pub fn enif_thread_type() c_int {
    return 0;
}
pub fn enif_time_offset(_: timeu) time {
    return .{};
}
pub fn enif_tsd_get(_: tsdk) c_int {
    return 0;
}
pub fn enif_tsd_key_destroy(_: tsdk) void {}
pub fn enif_tsd_set(_: tsdk, _: ?*anyopaque) void {}
// pub fn enif_vfprintf(...)
// pub fn enif_vsnprintf(...)
pub fn enif_whereis_pid(_: env, _: term, _: pid) c_int {
    return 0;
}
pub fn enif_whereis_port(_: env, _: term, _: pid) c_int {
    return 0;
}

// #define constants section.
// these values have to exist as exported const values.  Because this is a stub
// file, the values probably don't have to match the "true" values.  However, they
// must be unique within each category.

pub const ERL_NIF_TERM_TYPE_ATOM = 0;
pub const ERL_NIF_TERM_TYPE_BITSTRING = 1;
pub const ERL_NIF_TERM_TYPE_FLOAT = 2;
pub const ERL_NIF_TERM_TYPE_FUN = 3;
pub const ERL_NIF_TERM_TYPE_INTEGER = 4;
pub const ERL_NIF_TERM_TYPE_LIST = 5;
pub const ERL_NIF_TERM_TYPE_MAP = 6;
pub const ERL_NIF_TERM_TYPE_PID = 7;
pub const ERL_NIF_TERM_TYPE_PORT = 8;
pub const ERL_NIF_TERM_TYPE_REFERENCE = 9;
pub const ERL_NIF_TERM_TYPE_TUPLE = 10;

pub const ERL_NIF_LATIN1 = 0;

pub const ERL_NIF_SEC = 0;
pub const ERL_NIF_MSEC = 1;
pub const ERL_NIF_USEC = 2;
pub const ERL_NIF_NSEC = 3;

pub const ERL_NIF_UNIQUE_POSITIVE = 0;
pub const ERL_NIF_UNIQUE_MONOTONIC = 1;

pub const ERL_NIF_INTERNAL_HASH = 0;
pub const ERL_NIF_PHASH2 = 1;

pub const ERL_NIF_RT_CREATE = 0;
