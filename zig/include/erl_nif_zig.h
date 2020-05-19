#ifndef ZIG_NIF_H
#define ZIG_NIF_H

typedef unsigned long long uint64_t;
typedef unsigned long long size_t;

struct enif_environment_t;
typedef struct enif_environment_t ErlNifEnv;
typedef uint64_t ErlNifTerm;
typedef uint64_t ErlNifUInt64;

typedef struct
{
    size_t size;
    unsigned char* data;

    /* Internals (avert your eyes) */
    void* ref_bin;
    /* for future additions to be ABI compatible (same struct size) */
    void* __spare__[2];
}ErlNifBinary;

typedef struct enif_func_t
{
    const char* name;
    unsigned arity;
    ErlNifTerm (*fptr)(ErlNifEnv* env, int argc, const ErlNifTerm argv[]);
    unsigned flags;
}ErlNifFunc;

typedef struct {
    int driver_major_version;
    int driver_minor_version;
    char *erts_version;
    char *otp_release;
    int thread_support;
    int smp_support;
    int async_threads;
    int scheduler_threads;
    int nif_major_version;
    int nif_minor_version;
    int dirty_scheduler_support;
}  ErlNifSysInfo;

typedef struct
{
    ErlNifTerm pid;  /* internal, may change */
} ErlNifPid;

typedef enum {
    ERL_NIF_INTERNAL_HASH = 1,
    ERL_NIF_PHASH2 = 2
} ErlNifHash;

typedef enum {
    ERL_NIF_LATIN1
} ErlNifCharEncoding;

// NIF Functions: memory management
export void *enif_alloc(size_t size);
export void enif_free(void* ptr);
export void *enif_realloc(void* ptr, size_t size);

// NIF Functions: resource management

typedef struct enif_resource_type_t ErlNifResourceType;
typedef void ErlNifResourceDtor(ErlNifEnv*, void*);
typedef enum {
    ERL_NIF_RT_CREATE = 1,
    ERL_NIF_RT_TAKEOVER = 2,
    ERL_NIF_BOTH = 3
} ErlNifResourceFlags;

export void *enif_alloc_resource(ErlNifResourceType*, size_t);
export ErlNifTerm enif_make_resource(ErlNifEnv *, void *);
export int enif_get_resource(ErlNifEnv*, ErlNifTerm, ErlNifResourceType*, void**);
export int enif_keep_resource(void *);
export void enif_release_resource(void *);

export ErlNifResourceType *enif_open_resource_type(
    ErlNifEnv*,
    const char*,
    const char*,
    ErlNifResourceDtor*,
    ErlNifResourceFlags,
    ErlNifResourceFlags*);

// NIF Functions: guards
export int enif_is_atom(ErlNifEnv*, ErlNifTerm);
export int enif_is_binary(ErlNifEnv*, ErlNifTerm);
export int enif_is_ref(ErlNifEnv*, ErlNifTerm);
export int enif_is_fun(ErlNifEnv*, ErlNifTerm);
export int enif_is_pid(ErlNifEnv*, ErlNifTerm);
export int enif_is_port(ErlNifEnv*, ErlNifTerm);
export int enif_is_identical(ErlNifTerm, ErlNifTerm);
export int enif_is_list(ErlNifEnv*, ErlNifTerm);
export int enif_is_tuple(ErlNifEnv*, ErlNifTerm);
export int enif_is_empty_list(ErlNifEnv*, ErlNifTerm);
export int enif_is_map(ErlNifEnv*, ErlNifTerm);

export int enif_is_number(ErlNifEnv*, ErlNifTerm term);
export int enif_compare(ErlNifTerm, ErlNifTerm);

// NIF Functions: binary manipulation
export int enif_inspect_binary(ErlNifEnv *, ErlNifTerm bin_term, ErlNifBinary *bin);
export int enif_alloc_binary(size_t size, ErlNifBinary *bin);
export int enif_realloc_binary(ErlNifBinary *bin, size_t size);
export int enif_release_binary(ErlNifBinary *bin);

// NIF Functions: get values
export int enif_get_int(ErlNifEnv *, ErlNifTerm, int * ip);
export int enif_get_uint(ErlNifEnv*, ErlNifTerm, unsigned* ip);
export int enif_get_long(ErlNifEnv*, ErlNifTerm, long* ip);
export int enif_get_ulong(ErlNifEnv *, ErlNifTerm, unsigned long * ip);
export int enif_get_int64(ErlNifEnv *, ErlNifTerm, long * ip);
export int enif_get_double(ErlNifEnv *, ErlNifTerm, double * dp);
export int enif_get_list_cell(ErlNifEnv *, ErlNifTerm, ErlNifTerm *head, ErlNifTerm *tail);
export int enif_get_list_length(ErlNifEnv*, ErlNifTerm, unsigned* len);
export int enif_get_tuple(ErlNifEnv *, ErlNifTerm, int* arity, const ErlNifTerm **array);
export int enif_get_string(ErlNifEnv*, ErlNifTerm, char* buf, unsigned len, ErlNifCharEncoding);
export int enif_get_atom_length(ErlNifEnv*, ErlNifTerm, unsigned* buf, ErlNifCharEncoding);
export int enif_get_atom(ErlNifEnv*, ErlNifTerm, char* buf, unsigned len, ErlNifCharEncoding);

// NIF Functions: make values
export ErlNifTerm enif_make_binary(ErlNifEnv *, ErlNifBinary *);
export ErlNifTerm enif_make_badarg(ErlNifEnv *);
export ErlNifTerm enif_make_int(ErlNifEnv *, int);
export ErlNifTerm enif_make_ulong(ErlNifEnv *, unsigned long);
export ErlNifTerm enif_make_double(ErlNifEnv *, double);
export ErlNifTerm enif_make_atom(ErlNifEnv *, const char*);
export ErlNifTerm enif_make_atom_len(ErlNifEnv*, const char*, size_t);
export int enif_make_existing_atom(ErlNifEnv *, const char*, ErlNifTerm *atom, ErlNifCharEncoding);
export ErlNifTerm enif_make_tuple(ErlNifEnv *, unsigned cnt, ...);
export ErlNifTerm enif_make_list(ErlNifEnv *, unsigned cnt, ...);
export ErlNifTerm enif_make_list_cell(ErlNifEnv *, ErlNifTerm car, ErlNifTerm cdr);
export ErlNifTerm enif_make_string(ErlNifEnv *, const char * string, ErlNifCharEncoding);
export ErlNifTerm enif_make_ref(ErlNifEnv *);
export ErlNifTerm enif_make_uint(ErlNifEnv *, unsigned i);
export ErlNifTerm enif_make_long(ErlNifEnv *, long i);
export ErlNifTerm enif_make_tuple_from_array(ErlNifEnv*, const ErlNifTerm arr[], unsigned cnt);
export ErlNifTerm enif_make_list_from_array(ErlNifEnv*, const ErlNifTerm arr[], unsigned cnt);
export ErlNifTerm enif_make_pid(ErlNifEnv *, const ErlNifPid *);
export unsigned char* enif_make_new_binary(ErlNifEnv*, size_t size, ErlNifTerm* termp);

// NIF Functions: etcetera
export void enif_system_info(ErlNifSysInfo * sip, size_t si_size);
export ErlNifTerm enif_raise_exception(ErlNifEnv *env, ErlNifTerm reason);
export int enif_term_to_binary(ErlNifEnv *env, ErlNifTerm term, ErlNifBinary *bin);
export size_t enif_binary_to_term(ErlNifEnv *env, const unsigned char* data, size_t sz, ErlNifTerm *term, unsigned int opts);
export ErlNifUInt64 enif_hash(ErlNifHash type, ErlNifTerm term, ErlNifUInt64 salt);

// NIF Functions: environments
export ErlNifEnv* enif_alloc_env(void);
export void enif_free_env(ErlNifEnv* env);
export void enif_clear_env(ErlNifEnv* env);

// NIF Functions: message passing
export int enif_send(ErlNifEnv *, const ErlNifPid*, ErlNifEnv *msg_env, ErlNifTerm msg);
export ErlNifTerm enif_make_copy(ErlNifEnv *, ErlNifTerm);
export ErlNifPid *enif_self(ErlNifEnv *, ErlNifPid *);
export int enif_get_local_pid(ErlNifEnv *, ErlNifTerm, ErlNifPid *pid);

// NIF: map stuff
export int enif_get_map_size(ErlNifEnv *, ErlNifTerm, size_t *size);
export ErlNifTerm enif_make_new_map(ErlNifEnv *);
export int enif_make_map_put(ErlNifEnv *, ErlNifTerm map_in, ErlNifTerm key, ErlNifTerm value, ErlNifTerm *map_out);
export int enif_get_map_value(ErlNifEnv *, ErlNifTerm map, ErlNifTerm key, ErlNifTerm *value);
export int enif_make_map_update(ErlNifEnv *, ErlNifTerm map_in, ErlNifTerm key, ErlNifTerm value, ErlNifTerm *map_out);
export int enif_make_map_remove(ErlNifEnv *, ErlNifTerm map_in, ErlNifTerm key, ErlNifTerm *map_out);

// MONITORS
typedef struct {
    unsigned char data[sizeof(void *)*4];
} ErlDrvMonitor;

typedef ErlDrvMonitor ErlNifMonitor;

// RESOURCE THINGS
typedef int ErlNifEvent;
typedef void ErlNifResourceDtor(ErlNifEnv*, void*);
typedef void ErlNifResourceStop(ErlNifEnv*, void*, ErlNifEvent, int is_direct_call);
typedef void ErlNifResourceDown(ErlNifEnv*, void*, ErlNifPid*, ErlNifMonitor*);

typedef struct {
    ErlNifResourceDtor* dtor;
    ErlNifResourceStop* stop;  /* at ERL_NIF_SELECT_STOP event */
    ErlNifResourceDown* down;  /* enif_monitor_process */
} ErlNifResourceTypeInit;

typedef struct enif_entry_t
{
    int major;
    int minor;
    const char* name;
    int num_of_funcs;
    ErlNifFunc* funcs;
    int  (*load)   (ErlNifEnv*, void** priv_data, ErlNifTerm load_info);
    int  (*reload) (ErlNifEnv*, void** priv_data, ErlNifTerm load_info);
    int  (*upgrade)(ErlNifEnv*, void** priv_data, void** old_priv_data, ErlNifTerm load_info);
    void (*unload) (ErlNifEnv*, void* priv_data);

    /* Added in 2.1 */
    const char* vm_variant;

    /* Added in 2.7 */
    unsigned options;   /* Unused. Can be set to 0 or 1 (dirty sched config) */

    /* Added in 2.12 */
    size_t sizeof_ErlNifResourceTypeInit;

    /* Added in 2.14 */
    const char* min_erts;
}ErlNifEntry;

#endif
