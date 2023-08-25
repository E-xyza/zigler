//! general-purpose BEAM mutex wrapped into the zig mutex interface

const e = @import("erl_nif");

const MutexError = error{CreationFail};

pub fn Mutex(comptime name_: []const u8) type {
    return struct {
        mutex_ref: ?*e.ErlNifMutex = null,
        const name: []const u8 = name_;
        const Self = @This();

        /// initializes the mutex.  Note this is failable.
        pub fn init(self: *Self) !void {
            if (self.mutex_ref) |_| {} else {
                self.mutex_ref =
                    e.enif_mutex_create(@constCast(name.ptr)) orelse
                    return MutexError.CreationFail;
            }
        }

        pub fn deinit(self: *Self) void {
            if (self.mutex_ref) |mutex| {
                e.enif_mutex_destroy(mutex);
                self.mutex_ref = null;
            }
        }

        /// Try to acquire the mutex without blocking. Returns false
        /// if the mutex is unavailable.
        pub fn tryLock(self: *Self) bool {
            return switch (e.enif_mutex_trylock(self.mutex_ref)) {
                0 => zero: {
                    e.enif_mutex_lock(self.mutex_ref);
                    break :zero true;
                },
                e.EBUSY => false,
                _ => unreachable,
            };
        }

        /// Acquire the mutex. Will deadlock if the mutex is already
        /// held by the calling thread.
        pub fn lock(self: *Self) void {
            e.enif_mutex_lock(self.mutex_ref);
        }

        pub fn unlock(self: *Self) void {
            e.enif_mutex_unlock(self.mutex_ref);
        }
    };
}
