//! general-purpose BEAM mutex wrapped into the zig mutex interface

const e = @import("erl_nif.zig");
usingnamespace @import("os/bits.zig");

const MutexError = error {
    CreationFail
};

pub fn BeamMutex(comptime name: []const u8) type {
    return struct {
        mutex_ref: ?*e.ErlNifMutex = null,
        const name: []const u8 = name;
        const Self = @This();

        /// initializes the mutex.  Note this is failable.
        pub fn init(self: *Self) !void {
            if (self.mutex_ref) |_| {} else {
                self.mutex_ref = e.enif_mutex_create(self.name) orelse
                    return MutexError.CreationFail;
            }
        }

        pub fn deinit(self: *Self) void {
            if (self.mutex_ref) |mutex| {
                e.enif_mutex_destroy(mutex);
                self.mutex_ref = null;
            }
        }

        pub const Held = struct {
            mutex: *Self,

            pub fn release(self: Held) void {
                if (self.mutex.mutex_ref) |mutex| {
                    e.enif_mutex_unlock(mutex);
                } else unreachable;
            }
        };

        /// Try to acquire the mutex without blocking. Returns null if
        /// the mutex is unavailable. Otherwise returns Held. Call
        /// release on Held.
        pub fn tryAcquire(self: *Self) ?Held {
            return switch (e.enif_mutex_trylock(mutex)) {
                0 => Held{.mutex = self},
                EBUSY => null,
                _ => unreachable,
            };
        }

        /// Acquire the mutex. Will deadlock if the mutex is already
        /// held by the calling thread.
        pub fn acquire(self: *Dummy) Held {
            e.enif_mutex_lock(mutex);
            return Held{.mutex = self};
        }
    };
};