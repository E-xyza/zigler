// precompiled.zig

const beam = @import("beam");
const e = @import("erl_nif");

fn add_one(env: beam.env, _: c_int, args: [*c]const e.ErlNifTerm) callconv(.c) e.ErlNifTerm {
    var input_value: c_int = undefined;
    _ = e.enif_get_int(env, args[0], &input_value);
    const return_value = input_value + 1;
    return e.enif_make_int(env, return_value);
}

var exported_nifs = [_]e.ErlNifFunc{.{ .name = "marshalled-add_one", .arity = 1, .fptr = add_one, .flags = 0 }};

const entry = e.ErlNifEntry{
    .major = 2,
    .minor = 17,
    .name = "Elixir.ZiglerTest.LocalPrecompiledTest",
    .num_of_funcs = exported_nifs.len,
    .funcs = &exported_nifs,
    .load = null,
    .reload = null, // never supported as of OTP 20
    .upgrade = beam.loader.blank_upgrade,
    .unload = beam.loader.blank_unload,
    .vm_variant = "beam.vanilla",
    .options = 1,
    .sizeof_ErlNifResourceTypeInit = @sizeOf(e.ErlNifResourceTypeInit),
    .min_erts = "erts-16.0.2",
};

export fn nif_init() *const e.ErlNifEntry {
    return &entry;
}

export const sema: [182:0]u8 linksection(".sema") = "{\"decls\":[],\"functions\":[{\"name\":\"add_one\",\"params\":[{\"bits\":32,\"signedness\":\"unsigned\",\"type\":\"integer\"}],\"return\":{\"bits\":32,\"signedness\":\"unsigned\",\"type\":\"integer\"}}],\"types\":[]}".*;
