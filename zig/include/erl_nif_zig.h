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

// NIF Functions: memory management
extern void *enif_alloc(size_t size);
extern void enif_free(void* ptr);
extern void *enif_realloc(void* ptr, size_t size);

// NIF Functions: guards
extern int enif_is_atom(ErlNifEnv*, ErlNifTerm);
extern int enif_is_binary(ErlNifEnv*, ErlNifTerm);
extern int enif_is_ref(ErlNifEnv*, ErlNifTerm);
extern int enif_is_fun(ErlNifEnv*, ErlNifTerm);
extern int enif_is_pid(ErlNifEnv*, ErlNifTerm);
extern int enif_is_port(ErlNifEnv*, ErlNifTerm);
extern int enif_is_identical(ErlNifTerm, ErlNifTerm);
extern int enif_is_list(ErlNifEnv*, ErlNifTerm);
extern int enif_is_tuple(ErlNifEnv*, ErlNifTerm);
extern int enif_is_empty_list(ErlNifEnv*, ErlNifTerm);
extern int enif_is_map(ErlNifEnv*, ErlNifTerm);

extern int enif_is_number(ErlNifEnv*, ErlNifTerm term);
extern int enif_compare(ErlNifTerm, ErlNifTerm);

// NIF Functions: binary manipulation
extern int enif_inspect_binary(ErlNifEnv *, ErlNifTerm bin_term, ErlNifBinary *bin);
extern int enif_alloc_binary(size_t size, ErlNifBinary *bin);
extern int enif_realloc_binary(ErlNifBinary *bin, size_t size);
extern int enif_release_binary(ErlNifBinary *bin);

// NIF Functions: get values
extern int enif_get_int(ErlNifEnv *, ErlNifTerm, int * ip);
extern int enif_get_uint(ErlNifEnv*, ErlNifTerm, unsigned* ip);
extern int enif_get_long(ErlNifEnv*, ErlNifTerm, long* ip);
extern int enif_get_ulong(ErlNifEnv *, ErlNifTerm, unsigned long * ip);
extern int enif_get_double(ErlNifEnv *, ErlNifTerm, double * dp);
extern int enif_get_list_cell(ErlNifEnv *, ErlNifTerm, ErlNifTerm *head, ErlNifTerm *tail);
extern int enif_get_list_length(ErlNifEnv*, ErlNifTerm, unsigned* len);
extern int enif_get_tuple(ErlNifEnv *, ErlNifTerm, int* arity, const ErlNifTerm **array);
extern int enif_get_string(ErlNifEnv*, ErlNifTerm list, char* buf, unsigned len, ErlNifCharEncoding);
extern int enif_get_atom(ErlNifEnv*, ErlNifTerm atom, char* buf, unsigned len, ErlNifCharEncoding);

// NIF Functions: make values
extern ErlNifTerm enif_make_binary(ErlNifEnv *, ErlNifBinary *);
extern ErlNifTerm enif_make_badarg(ErlNifEnv *);
extern ErlNifTerm enif_make_int(ErlNifEnv *, int);
extern ErlNifTerm enif_make_ulong(ErlNifEnv *, unsigned long);
extern ErlNifTerm enif_make_double(ErlNifEnv *, double);
extern ErlNifTerm enif_make_atom(ErlNifEnv *, const char*);
extern ErlNifTerm enif_make_atom_len(ErlNifEnv*, const char*, size_t);
extern int enif_make_exisiting_atom(ErlNifEnv *, const char*, ErlNifTerm *atom, ErlNifCharEncoding);
extern ErlNifTerm enif_make_tuple(ErlNifEnv *, unsigned cnt, ...);
extern ErlNifTerm enif_make_list(ErlNifEnv *, unsigned cnt, ...);
extern ErlNifTerm enif_make_list_cell(ErlNifEnv *, ErlNifTerm car, ErlNifTerm cdr);
extern ErlNifTerm enif_make_string(ErlNifEnv *, const char * string, ErlNifCharEncoding);
extern ErlNifTerm enif_make_ref(ErlNifEnv *);
extern ErlNifTerm enif_make_uint(ErlNifEnv*, unsigned i);
extern ErlNifTerm enif_make_long(ErlNifEnv*, long i);
extern ErlNifTerm enif_make_tuple_from_array(ErlNifEnv*, const ErlNifTerm arr[], unsigned cnt);
extern ErlNifTerm enif_make_list_from_array(ErlNifEnv*, const ErlNifTerm arr[], unsigned cnt);
extern unsigned char* enif_make_new_binary(ErlNifEnv*, size_t size, ErlNifTerm* termp);

// NIF Functions: etcetera
extern void enif_system_info(ErlNifSysInfo * sip, size_t si_size);
extern ErlNifTerm enif_raise_exception(ErlNifEnv *env, ErlNifTerm reason);
extern int enif_term_to_binary(ErlNifEnv *env, ErlNifTerm term, ErlNifBinary *bin);
extern size_t enif_binary_to_term(ErlNifEnv *env, const unsigned char* data, size_t sz, ErlNifTerm *term, unsigned int opts);
extern ErlNifUInt64 enif_hash(ErlNifHash type, ErlNifTerm term, ErlNifUInt64 salt);

// NIF Functions: environments
extern ErlNifEnv* enif_alloc_env(void);
extern void enif_free_env(ErlNifEnv* env);
extern void enif_clear_env(ErlNifEnv* env);

// NIF Functions: message passing
extern int enif_send(ErlNifEnv *, const ErlNifPid*, ErlNifEnv *msg_env, ErlNifTerm msg);
extern ErlNifTerm enif_make_copy(ErlNifEnv *, ErlNifTerm);
extern ErlNifPid *enif_self(ErlNifEnv *, ErlNifPid *);
extern int enif_get_local_pid(ErlNifEnv *, ErlNifTerm, ErlNifPid *pid);

// NIF: map stuff
extern int enif_get_map_size(ErlNifEnv *, ErlNifTerm, size_t *size);
extern ErlNifTerm enif_make_new_map(ErlNifEnv *);
extern int enif_make_map_put(ErlNifEnv *, ErlNifTerm map_in, ErlNifTerm key, ErlNifTerm value, ErlNifTerm *map_out);
extern int enif_get_map_value(ErlNifEnv *, ErlNifTerm map, ErlNifTerm key, ErlNifTerm *value);
extern int enif_make_map_update(ErlNifEnv *, ErlNifTerm map_in, ErlNifTerm key, ErlNifTerm value, ErlNifTerm *map_out);
extern int enif_make_map_remove(ErlNifEnv *, ErlNifTerm map_in, ErlNifTerm key, ErlNifTerm *map_out);

typedef struct enif_resource_type_t ErlNifResourceType;

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