//
// Fuserl NIF variant
//
#include <stdio.h>
#include <stdint.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <poll.h>

#include <sys/sysmacros.h>
#include <sys/mount.h>
#include <sys/param.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/types.h>
#if HAVE_SETXATTR
#include <sys/xattr.h>
#endif

#include "erl_nif.h"
#include "erl_driver.h"

#include <fuse/fuse_lowlevel.h>
#include "fuserl_portability.h"
#include "fuserltypes.h"

// #define DEBUG
// #define NIF_TRACE

#define UNUSED(a) ((void) a)

#include "debug.h"
#include "atom_hash.h"

#define ATOM(name) atm_##name

#define DECL_ATOM(name) \
    ERL_NIF_TERM atm_##name = 0

// require env in context (ugly)
#define LOAD_ATOM(name)			\
    atm_##name = enif_make_atom(env,#name)

#define LOAD_ATOM_STRING(name,string)		\
    atm_##name = enif_make_atom(env,string)

static int load(ErlNifEnv* env, void** priv_data,
		       ERL_NIF_TERM load_info);
static int upgrade(ErlNifEnv* env, void** priv_data,
			  void** old_priv_data,
		       ERL_NIF_TERM load_info);
static void unload(ErlNifEnv* env, void* priv_data);

// Dirty optional since 2.7 and mandatory since 2.12
#if (ERL_NIF_MAJOR_VERSION > 2) || ((ERL_NIF_MAJOR_VERSION == 2) && (ERL_NIF_MINOR_VERSION >= 7))
#ifdef USE_DIRTY_SCHEDULER
#define NIF_FUNC(name,arity,fptr) {(name),(arity),(fptr),(ERL_NIF_DIRTY_JOB_CPU_BOUND)}
#define NIF_DIRTY_FUNC(name,arity,fptr) {(name),(arity),(fptr),(ERL_NIF_DIRTY_JOB_CPU_BOUND)}
#else
#define NIF_FUNC(name,arity,fptr) {(name),(arity),(fptr),(0)}
#define NIF_DIRTY_FUNC(name,arity,fptr) {(name),(arity),(fptr),(ERL_NIF_DIRTY_JOB_CPU_BOUND)}
#endif
#else
#define NIF_FUNC(name,arity,fptr) {(name),(arity),(fptr)}
#define NIF_DIRTY_FUNC(name,arity,fptr) {(name),(arity),(fptr)}
#endif

#define NIF_LIST \
    NIF("mount", 3,    nif_fuserl_mount)  \
    NIF("unmount", 1,  nif_fuserl_unmount) \
    NIF("process", 1,  nif_fuserl_process) \
    NIF("reply", 3,    nif_fuserl_reply)

static ErlNifResourceType* fuserl_r;

typedef struct 
{
    char*                     mountpoint;
    char*                     mountopts;
    struct fuse_chan*         chan;
    struct fuse_session*      se;
    struct fuse_lowlevel_ops  op;
    char*                     buf;
    ErlNifEnv*                env;
    ErlNifPid                 pid;
} fuserl_ctx_t;

DECL_ATOM(ok);
DECL_ATOM(error);
DECL_ATOM(false);
DECL_ATOM(true);
DECL_ATOM(eof);
DECL_ATOM(undefined);
DECL_ATOM(select);
DECL_ATOM(fuserl);
// types
DECL_ATOM(fuse_file_info);
DECL_ATOM(fuse_entry_param);
DECL_ATOM(fuse_ctx);
DECL_ATOM(stat);
DECL_ATOM(statvfs);
DECL_ATOM(flock);
DECL_ATOM(direntry);
DECL_ATOM(fuse_forget_data);
// operations
DECL_ATOM(init);
DECL_ATOM(destroy);
DECL_ATOM(lookup);
DECL_ATOM(forget);
DECL_ATOM(getattr);
DECL_ATOM(setattr);
DECL_ATOM(readlink);
DECL_ATOM(mknod);
DECL_ATOM(mkdir);
DECL_ATOM(unlink);
DECL_ATOM(rmdir);
DECL_ATOM(symlink);
DECL_ATOM(rename);
DECL_ATOM(link);
DECL_ATOM(open);
DECL_ATOM(read);
DECL_ATOM(write);
DECL_ATOM(flush);
DECL_ATOM(release);
DECL_ATOM(fsync);
DECL_ATOM(opendir);
DECL_ATOM(readdir);
DECL_ATOM(releasedir);
DECL_ATOM(fsyncdir);
DECL_ATOM(statfs);
DECL_ATOM(setxattr);
DECL_ATOM(getxattr);
DECL_ATOM(listxattr);
DECL_ATOM(removexattr);
DECL_ATOM(access);
DECL_ATOM(create);
DECL_ATOM(getlk);
DECL_ATOM(setlk);
DECL_ATOM(bmap);
DECL_ATOM(ioctl);
DECL_ATOM(poll);
DECL_ATOM(write_buf);
DECL_ATOM(retrieve_reply);
DECL_ATOM(forget_multi);
//DECL_ATOM(flock);
DECL_ATOM(fallocate);
// reply
DECL_ATOM(fuse_reply_err);
DECL_ATOM(fuse_reply_none);
DECL_ATOM(fuse_reply_entry);
DECL_ATOM(fuse_reply_create);
DECL_ATOM(fuse_reply_attr);
DECL_ATOM(fuse_reply_readlink);
DECL_ATOM(fuse_reply_open);
DECL_ATOM(fuse_reply_write);
DECL_ATOM(fuse_reply_buf);
DECL_ATOM(fuse_reply_statfs);
DECL_ATOM(fuse_reply_xattr);
DECL_ATOM(fuse_reply_lock);
DECL_ATOM(fuse_reply_direntrylist);


static helem_t reply_tab[] = {
    HELEM(ATOM(fuse_reply_err), FUSE_REPLY_ERR),
    HELEM(ATOM(fuse_reply_none), FUSE_REPLY_NONE),
    HELEM(ATOM(fuse_reply_entry), FUSE_REPLY_ENTRY),
    HELEM(ATOM(fuse_reply_create), FUSE_REPLY_CREATE),
    HELEM(ATOM(fuse_reply_attr), FUSE_REPLY_ATTR),
    HELEM(ATOM(fuse_reply_readlink), FUSE_REPLY_READLINK),
    HELEM(ATOM(fuse_reply_open), FUSE_REPLY_OPEN),
    HELEM(ATOM(fuse_reply_write), FUSE_REPLY_WRITE),
    HELEM(ATOM(fuse_reply_buf), FUSE_REPLY_BUF),
    HELEM(ATOM(fuse_reply_statfs), FUSE_REPLY_STATFS),
    HELEM(ATOM(fuse_reply_xattr), FUSE_REPLY_XATTR),
    HELEM(ATOM(fuse_reply_lock), FUSE_REPLY_LOCK),
    HELEM(ATOM(fuse_reply_direntrylist),FUSE_REPLY_DIRENTRYLIST),
    { NULL, NULL, 0, 0 }
};

#define REPLY_HASH_SIZE 61
static helem_t* reply_hash[REPLY_HASH_SIZE];

// Declare all nif functions
#undef NIF
#ifdef NIF_TRACE
#define NIF(name, arity, func)						\
    static ERL_NIF_TERM func(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]); \
    static ERL_NIF_TERM trace##_##func##_##arity(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]);
#else
#define NIF(name, arity, func)						\
    static ERL_NIF_TERM func(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]);
#endif

NIF_LIST

#undef NIF
#ifdef NIF_TRACE
#define NIF(name,arity,func) NIF_FUNC(name, arity, trace##_##func##_##arity),
#else
#define NIF(name,arity,func) NIF_FUNC(name, arity, func),
#endif

static ErlNifFunc nif_funcs[] =
{
    NIF_LIST
};

// check if atm is member in list
static int atom_member(ErlNifEnv* env, ERL_NIF_TERM atm, ERL_NIF_TERM list)
{
    ERL_NIF_TERM head, tail;
    while(enif_get_list_cell(env, list, &head, &tail)) {
	if (head == atm) return 1;
	list = tail;
    }
    return 0;
}

static int get_uint64(ErlNifEnv* env, ERL_NIF_TERM arg, void* ptr)
{
    uint64_t val;
    if (!enif_get_uint64(env, arg, &val)) return 0;
    *((uint64_t*)ptr) = val;
    return 1;
}

static int get_int64(ErlNifEnv* env, ERL_NIF_TERM arg, void* ptr)
{
    int64_t val;
    if (!enif_get_int64(env, arg, &val)) return 0;
    *((int64_t*)ptr) = val;
    return 1;
}

static ERL_NIF_TERM make_pointer(ErlNifEnv* env, void* ptr)
{
    return enif_make_uint64(env, (uintptr_t)(ptr));
}

static int get_pointer(ErlNifEnv* env, ERL_NIF_TERM arg, void** ptr_ptr)
{
    uint64_t uval;
    if (!enif_get_uint64(env, arg, &uval)) return 0;
    *ptr_ptr = (void*)((uintptr_t) uval);
    return 1;
}

static ERL_NIF_TERM make_boolean(ErlNifEnv* env, int bool)
{
    return bool ? ATOM(true) : ATOM(false);
}


// get iolist as dynamically allocated string
static int get_string(ErlNifEnv* env, ERL_NIF_TERM arg, char** str_ptr)
{
    ErlNifBinary bin;
    char* ptr;
    if (!enif_inspect_iolist_as_binary(env, arg, &bin))
	return 0;
    if ((ptr = enif_alloc(bin.size+1)) == NULL)
	return 0;
    memcpy(ptr, bin.data, bin.size);
    ptr[bin.size] = '\0';
    *str_ptr = ptr;
    return 1;
}

// make uid/gid/pid that may be 32 bit, preserv all bit set!
static ERL_NIF_TERM make_uid(ErlNifEnv* env, uid_t value)
{
    if (value == (uid_t) -1)
	return enif_make_int(env, -1);
    else
	return enif_make_uint64(env, value);
}

static int get_uid(ErlNifEnv* env, ERL_NIF_TERM arg, uid_t* uid_ptr)
{
    int64_t val;
    if (!enif_get_int64(env, arg, &val)) return 0;
    *uid_ptr = (uid_t) val;
    return 1;
}

static ERL_NIF_TERM make_gid(ErlNifEnv* env, gid_t value)
{
    if (value == (gid_t) -1)
	return enif_make_int(env, -1);
    else
	return enif_make_uint64(env, value);
}

static int get_gid(ErlNifEnv* env, ERL_NIF_TERM arg, gid_t* gid_ptr)
{
    int64_t val;
    if (!enif_get_int64(env, arg, &val)) return 0;
    *gid_ptr = (gid_t) val;
    return 1;
}

static ERL_NIF_TERM make_pid(ErlNifEnv* env, pid_t value)
{
    if (value == (pid_t) -1)
	return enif_make_int(env, -1);
    else
	return enif_make_uint64(env, value);
}

static int get_pid(ErlNifEnv* env, ERL_NIF_TERM arg, pid_t* pid_ptr)
{
    int64_t val;
    if (!enif_get_int64(env, arg, &val)) return 0;
    *pid_ptr = (pid_t) val;
    return 1;
}

static ERL_NIF_TERM make_ctx(ErlNifEnv* env, const struct fuse_ctx* ctx)
{
    if (ctx == NULL)
	return ATOM(undefined);
    else
	return enif_make_tuple(
	    env, 4,
	    ATOM(fuse_ctx),
	    make_uid(env, ctx->uid),
	    make_gid(env, ctx->gid),
	    make_pid(env, ctx->pid));
}

// used in reply to fuse
static int get_fuse_file_info(ErlNifEnv* env, ERL_NIF_TERM arg,
			      struct fuse_file_info* fi)
{
    int arity;
    const ERL_NIF_TERM* elem;
    int ival;
    unsigned int uval;
    
    if (!enif_get_tuple(env, arg, &arity, &elem) || (arity != 8))
	return 0;
    if (elem[0] != ATOM(fuse_file_info)) return 0;
    memset(fi, 0, sizeof(struct fuse_file_info));
    if (!enif_get_int(env, elem[1], &ival)) return 0;
    fi->flags = fuserl_open_flags_decanonicalize(ival);  // make native
    // ignore fi->fh_old
    if (!enif_get_int(env, elem[2],  &fi->writepage)) return 0;
    if (!enif_get_uint(env, elem[3], &uval)) return 0;
    fi->direct_io = uval;
    if (!enif_get_uint(env, elem[4], &uval)) return 0;
    fi->keep_cache = uval;
    if (!enif_get_uint(env, elem[5], &uval)) return 0;
    fi->flush = uval;
    // nonseekable (2.8)
    // flock_release (2.9)
    if (!enif_get_uint64(env, elem[6], &fi->fh)) return 0;
    if (!enif_get_uint64(env, elem[7], &fi->lock_owner)) return 0;
    return 1;
}

// decode fuse_file_info into erlang struct
static ERL_NIF_TERM make_fuse_file_info(ErlNifEnv* env,
					struct fuse_file_info* fi)
{
    if (fi == NULL)
	return ATOM(undefined);
    else {
	int flags = fuserl_open_flags_canonicalize(fi->flags);
	return enif_make_tuple(
	    env, 8,
	    ATOM(fuse_file_info),
	    enif_make_int(env, flags),
	    enif_make_int(env, fi->writepage),
	    enif_make_uint(env, fi->direct_io),
	    enif_make_uint(env, fi->keep_cache),
	    enif_make_uint(env, fi->flush),
	    // nonseekable (2.8)
	    // flock_release (2.9)
	    enif_make_uint64(env, fi->fh),
	    enif_make_uint64(env, fi->lock_owner));
    }
}

static int get_dev(ErlNifEnv* env, ERL_NIF_TERM arg, dev_t* dp)
{
    int arity;    
    const ERL_NIF_TERM* dev;
    uint64_t a, b;
    
    if (!enif_get_tuple(env, arg, &arity, &dev) || (arity != 2)) return 0;
    if (!enif_get_uint64(env, dev[0], &a)) return 0;
    if (!enif_get_uint64(env, dev[1], &b)) return 0;
    *dp = makedev (a, b);
    return 1;
}

static ERL_NIF_TERM make_dev(ErlNifEnv* env, dev_t d)
{
    return enif_make_tuple2(env,
			    enif_make_uint64(env, major(d)),
			    enif_make_uint64(env, minor(d)));
}

static int get_timeout(ErlNifEnv* env, ERL_NIF_TERM arg, double* tmo_ptr)
{
    double tm;
    uint64_t tmu;
    
    if (enif_get_double(env, arg, &tm)) {
	*tmo_ptr = tm/1000.0;
	return 1;
    }
    else if (enif_get_uint64(env, arg, &tmu)) {
	*tmo_ptr = tmu/1000.0;
	return 1;
    }
    return 0;
}


static int get_stat(ErlNifEnv* env, ERL_NIF_TERM arg, struct stat* sp)
{
    int arity;
    const ERL_NIF_TERM* elem;
    uint64_t m;

    if (!enif_get_tuple(env, arg, &arity, &elem) || (arity != 14))
	return 0;
    if (elem[0] != ATOM(stat)) return 0;
    memset(sp, 0, sizeof(struct stat));
    if (!get_dev(env, elem[1], &sp->st_dev)) return 0;
    if (!enif_get_uint64(env, elem[2], &sp->st_ino)) return 0;
    if (!enif_get_uint64(env, elem[3], &m)) return 0;
    sp->st_mode = fuserl_stat_mode_decanonicalize(m);
    if (!enif_get_uint64(env, elem[4], &sp->st_nlink)) return 0;    
    if (!get_uid(env,         elem[5], &sp->st_uid)) return 0;
    if (!get_gid(env,         elem[6], &sp->st_gid)) return 0;
    if (!get_dev(env,         elem[7], &sp->st_rdev)) return 0;
    // fixme: get_uint64 is unsafe (kind of)
    if (!get_uint64(env,      elem[8], &sp->st_size)) return 0;
    if (!get_uint64(env,      elem[9], &sp->st_blksize)) return 0;
    if (!get_uint64(env,      elem[10], &sp->st_blocks)) return 0;
    if (!get_uint64(env,      elem[11], &sp->st_atime)) return 0;
    if (!get_uint64(env,      elem[12], &sp->st_mtime)) return 0;
    if (!get_uint64(env,      elem[13], &sp->st_ctime)) return 0;
    return 1;
}

static ERL_NIF_TERM make_stat(ErlNifEnv* env, struct stat* sp)
{
    return enif_make_tuple(
	env, 14,
	ATOM(stat),
	make_dev(env, sp->st_dev),
	enif_make_uint64(env, sp->st_ino),
	enif_make_uint64(env, fuserl_stat_mode_canonicalize(sp->st_mode)),
	enif_make_uint64(env, sp->st_nlink),
	make_uid(env,  sp->st_uid),
	make_gid(env,  sp->st_gid),
	make_dev(env,    sp->st_rdev),
	enif_make_uint64(env, sp->st_size),
	enif_make_uint64(env, sp->st_blksize),
	enif_make_uint64(env, sp->st_blocks),
	enif_make_uint64(env, sp->st_atime),
	enif_make_uint64(env, sp->st_mtime),
	enif_make_uint64(env, sp->st_ctime));
}

static int get_statvfs(ErlNifEnv* env, ERL_NIF_TERM arg, struct statvfs* vp)
{
    int arity;
    const ERL_NIF_TERM* elem;

    if (!enif_get_tuple(env, arg, &arity, &elem) || (arity != 12))
	return 0;
    if (elem[0] != ATOM(statvfs)) return 0;    
    memset(vp, 0, sizeof(struct statvfs));
    if (!enif_get_uint64(env, elem[1], &vp->f_bsize)) return 0;
    if (!enif_get_uint64(env, elem[2], &vp->f_frsize)) return 0;
    if (!enif_get_uint64(env, elem[3], &vp->f_blocks)) return 0;
    if (!enif_get_uint64(env, elem[4], &vp->f_bfree)) return 0;
    if (!enif_get_uint64(env, elem[5], &vp->f_bavail)) return 0;
    if (!enif_get_uint64(env, elem[6], &vp->f_files)) return 0;
    if (!enif_get_uint64(env, elem[7], &vp->f_ffree)) return 0;
    if (!enif_get_uint64(env, elem[8], &vp->f_favail)) return 0;
    if (!enif_get_uint64(env, elem[9], &vp->f_fsid)) return 0;
    if (!enif_get_uint64(env, elem[10], &vp->f_flag)) return 0;
    if (!enif_get_uint64(env, elem[11], &vp->f_namemax)) return 0;
    return 1;
}

static ERL_NIF_TERM make_statvfs(ErlNifEnv* env, struct statvfs* vp)
{
    return enif_make_tuple(
	env, 12,
	ATOM(statvfs),
	enif_make_uint64(env, vp->f_bsize),
	enif_make_uint64(env, vp->f_frsize),
	enif_make_uint64(env, vp->f_blocks),
	enif_make_uint64(env, vp->f_bfree),
	enif_make_uint64(env, vp->f_bavail),
	enif_make_uint64(env, vp->f_files),
	enif_make_uint64(env, vp->f_ffree),
	enif_make_uint64(env, vp->f_favail),
	enif_make_uint64(env, vp->f_fsid),
	enif_make_uint64(env, vp->f_flag),
	enif_make_uint64(env, vp->f_namemax));
}

static int get_fuse_entry_param(ErlNifEnv* env, ERL_NIF_TERM arg,
				struct fuse_entry_param* param_ptr)
{
    int arity;
    const ERL_NIF_TERM* elem;
    
    if (!enif_get_tuple(env, arg, &arity, &elem) || (arity != 6))
	return 0;
    if (elem[0] != ATOM(fuse_entry_param)) return 0;
    if (!enif_get_uint64(env, elem[1], &param_ptr->ino)) return 0;
    if (!enif_get_uint64(env, elem[2], &param_ptr->generation)) return 0;
    if (elem[3] != ATOM(undefined)) {
	if (!get_stat(env, elem[3], &param_ptr->attr)) return 0;
    }
    if (!get_timeout(env, elem[4], &param_ptr->attr_timeout)) return 0;
    if (!get_timeout(env, elem[5], &param_ptr->entry_timeout)) return 0;    
    return 1;
}

// /usr/include/bits/fcntl.h

static int get_flock(ErlNifEnv* env, ERL_NIF_TERM arg,
		     struct flock* fp)
{
    int arity;
    const ERL_NIF_TERM* elem;
    int val;
    
    if (!enif_get_tuple(env, arg, &arity, &elem) || (arity != 6))
	return 0;
    if (elem[0] != ATOM(flock)) return 0;
    memset(fp, 0, sizeof(struct flock));    
    if (!enif_get_int(env, elem[1], &val)) return 0;
    fp->l_type = fuserl_l_type_decanonicalize(val);
    if (!enif_get_int(env, elem[2], &val)) return 0;
    fp->l_whence = fuserl_l_whence_decanonicalize(val);
    if (!get_uint64(env, elem[3], &fp->l_start)) return 0;
    if (!get_uint64(env, elem[4], &fp->l_len)) return 0;
    if (!get_pid(env, elem[4], &fp->l_pid)) return 0;
    return 1;
}

static ERL_NIF_TERM make_flock(ErlNifEnv* env, struct flock* fp)
{
    return enif_make_tuple6(
	env,
	ATOM(flock),
	enif_make_int(env,fuserl_l_type_canonicalize(fp->l_type)),
	enif_make_int(env,fuserl_l_whence_canonicalize(fp->l_whence)),
	enif_make_uint64(env, fp->l_start),
	enif_make_uint64(env, fp->l_len),
	make_pid(env, fp->l_pid));
}

// {fuserl, Req, Ctx, Op, Args}
static ERL_NIF_TERM make_msg(ErlNifEnv* env, fuse_req_t req,
			     ERL_NIF_TERM op, ERL_NIF_TERM args)
{
    const struct fuse_ctx* ctx = req ? fuse_req_ctx(req) : 0;
    return enif_make_tuple(
	env, 5,
	ATOM(fuserl),
	make_pointer(env, req),
	make_ctx(env, ctx),
	op,
	args);
}

static int send_msg(ErlNifEnv* env, ErlNifPid* pid,
		    fuse_req_t req, ERL_NIF_TERM op, ERL_NIF_TERM args)
{
    ERL_NIF_TERM msg = make_msg(env, req, op, args);
    DEBUGF("send_msg: req=%ld, op=%T\r\n", req, op);
    if (enif_send(NULL, pid, env, msg)) {
	enif_clear_env(env);
	return 1;
    }
    return 0;
}


static ERL_NIF_TERM make_error(ErlNifEnv* env, int e)
{
    return enif_make_tuple2(env,
			    ATOM(error),
			    enif_make_atom(env, erl_errno_id(e)));
}

static void fuserl_dtor(ErlNifEnv* env, fuserl_ctx_t* ctx)
{
    UNUSED(env);
    DEBUGF("fuserl_dtor", "");
}

static void fuserl_stop(ErlNifEnv* env, fuserl_ctx_t* dp,
		      ErlNifEvent event, int is_direct_call)
{
    UNUSED(env);
    DEBUGF("fuserl_stop", "");
}

static void fuserl_down(ErlNifEnv* env, fuserl_ctx_t* dp,
			const ErlNifPid* pid, const ErlNifMonitor* mon)
{
    UNUSED(env);
    DEBUGF("fuserl_down", "");
}


//////////////////////////////////////////////////////////////////////////////
/// FS WRAPPERS
//////////////////////////////////////////////////////////////////////////////

/**
 * Initialize filesystem
 *
 * Called before any other filesystem method
 *
 * There's no reply to this function
 *
 * @param userdata the user data passed to fuse_lowlevel_new()
 */
static void fuse_op_init(void *userdata, struct fuse_conn_info *conn)
{
    fuserl_ctx_t* nif_ctx = (fuserl_ctx_t*) userdata;
    ERL_NIF_TERM args;
    ErlNifEnv* env = nif_ctx->env;

    DEBUGF("op:init\r\n", "");

    args = enif_make_list(env, 0);
    send_msg(env, &nif_ctx->pid, (fuse_req_t) 0, ATOM(init), args);
}

/**
 * Clean up filesystem
 *
 * Called on filesystem exit
 *
 * There's no reply to this function
 *
 * @param userdata the user data passed to fuse_lowlevel_new()
 */
// terminate
static void fuse_op_destroy(void *userdata)
{
    fuserl_ctx_t* nif_ctx = (fuserl_ctx_t*) userdata;
    ERL_NIF_TERM args;
    ErlNifEnv* env = nif_ctx->env;

    DEBUGF("op:destroy\r\n", "");

    args = enif_make_list(env, 0);
    send_msg(env, &nif_ctx->pid, (fuse_req_t) 0, ATOM(destroy), args);
}

/**
 * Look up a directory entry by name and get its attributes.
 *
 * Valid replies:
 *   fuse_reply_entry
 *   fuse_reply_err
 *
 * @param req request handle
 * @param parent inode number of the parent directory
 * @param name the name to look up
 */

static void fuse_op_lookup(fuse_req_t req,
			   fuse_ino_t parent, const char *name)
{
    fuserl_ctx_t* nif_ctx = fuse_req_userdata(req);
    ERL_NIF_TERM args;
    ErlNifEnv* env = nif_ctx->env;

    DEBUGF("op:lookup\r\n", "");    

    args = enif_make_list2(env,
	enif_make_uint64(env, parent),
	enif_make_string(env, name, ERL_NIF_LATIN1)
	);
    send_msg(env, &nif_ctx->pid, req, ATOM(lookup), args);
}

/**
 * Forget about an inode
 *
 * This function is called when the kernel removes an inode
 * from its internal caches.
 *
 * The inode's lookup count increases by one for every call to
 * fuse_reply_entry and fuse_reply_create. The nlookup parameter
 * indicates by how much the lookup count should be decreased.
 *
 * Inodes with a non-zero lookup count may receive request from
 * the kernel even after calls to unlink, rmdir or (when
 * overwriting an existing file) rename. Filesystems must handle
 * such requests properly and it is recommended to defer removal
 * of the inode until the lookup count reaches zero. Calls to
 * unlink, remdir or rename will be followed closely by forget
 * unless the file or directory is open, in which case the
 * kernel issues forget only after the release or releasedir
 * calls.
 *
 * Note that if a file system will be exported over NFS the
 * inodes lifetime must extend even beyond forget. See the
 * generation field in struct fuse_entry_param above.
 *
 * On unmount the lookup count for all inodes implicitly drops
 * to zero. It is not guaranteed that the file system will
 * receive corresponding forget messages for the affected
 * inodes.
 *
 * Valid replies:
 *   fuse_reply_none
 *
 * @param req request handle
 * @param ino the inode number
 * @param nlookup the number of lookups to forget
 */
static void fuse_op_forget(fuse_req_t req,
			   fuse_ino_t ino, unsigned long nlookup)
{
    fuserl_ctx_t* nif_ctx = fuse_req_userdata(req);
    ERL_NIF_TERM args;
    ErlNifEnv* env = nif_ctx->env;

    DEBUGF("op:forget\r\n", "");    

    args = enif_make_list2(env,
			  enif_make_uint64(env, ino),
			  enif_make_uint64(env, nlookup) );
    send_msg(env, &nif_ctx->pid, req, ATOM(forget), args);
}

/**
 * Get file attributes
 *
 * Valid replies:
 *   fuse_reply_attr
 *   fuse_reply_err
 *
 * @param req request handle
 * @param ino the inode number
 * @param fi for future use, currently always NULL
 */
static void fuse_op_getattr(fuse_req_t req, fuse_ino_t ino,
			    struct fuse_file_info *fi)
{
    fuserl_ctx_t* nif_ctx = fuse_req_userdata(req);
    ERL_NIF_TERM args;
    ErlNifEnv* env = nif_ctx->env;

    DEBUGF("op:getattr req=%x ino=%ld, fi=%x\r\n", req, ino, fi);

    args = enif_make_list1(env,
			   enif_make_uint64(env, ino));
                           // make_fuse_file_info(env, fi) // NOT used
    send_msg(env, &nif_ctx->pid, req, ATOM(getattr), args);
}

/**
 * Set file attributes
 *
 * In the 'attr' argument only members indicated by the 'to_set'
 * bitmask contain valid values.  Other members contain undefined
 * values.
 *
 * If the setattr was invoked from the ftruncate() system call
 * under Linux kernel versions 2.6.15 or later, the fi->fh will
 * contain the value set by the open method or will be undefined
 * if the open method didn't set any value.  Otherwise (not
 * ftruncate call, or kernel version earlier than 2.6.15) the fi
 * parameter will be NULL.
 *
 * Valid replies:
 *   fuse_reply_attr
 *   fuse_reply_err
 *
 * @param req request handle
 * @param ino the inode number
 * @param attr the attributes
 * @param to_set bit mask of attributes which should be set
 * @param fi file information, or NULL
 *
 * Changed in version 2.5:
 *     file information filled in for ftruncate
 */
static void fuse_op_setattr(fuse_req_t req, fuse_ino_t ino, struct stat *attr,
			    int to_set, struct fuse_file_info *fi)
{
    fuserl_ctx_t* nif_ctx = fuse_req_userdata(req);
    ERL_NIF_TERM args;
    ErlNifEnv* env = nif_ctx->env;

    DEBUGF("op:setattr\r\n", "");
    
    args = enif_make_list4(env,
			  enif_make_uint64(env, ino),
			  make_stat(env, attr),
			  enif_make_int(env, to_set),
			  make_fuse_file_info(env, fi));
    send_msg(env, &nif_ctx->pid, req, ATOM(setattr), args);
}

/**
 * Read symbolic link
 *
 * Valid replies:
 *   fuse_reply_readlink
 *   fuse_reply_err
 *
 * @param req request handle
 * @param ino the inode number
 */
static void fuse_op_readlink(fuse_req_t req, fuse_ino_t ino)
{
    fuserl_ctx_t* nif_ctx = fuse_req_userdata(req);
    ErlNifEnv* env = nif_ctx->env;
    ERL_NIF_TERM args;

    DEBUGF("op:readlink\r\n", "");

    args = enif_make_list1(env,
			  enif_make_uint64(env, ino)
	);
    send_msg(env, &nif_ctx->pid, req, ATOM(readlink), args);
}

/**
 * Create file node
 *
 * Create a regular file, character device, block device, fifo or
 * socket node.
 *
 * Valid replies:
 *   fuse_reply_entry
 *   fuse_reply_err
 *
 * @param req request handle
 * @param parent inode number of the parent directory
 * @param name to create
 * @param mode file type and mode with which to create the new file
 * @param rdev the device number (only valid if created file is a device)
 */
static void fuse_op_mknod(fuse_req_t req, fuse_ino_t parent, const char *name,
			  mode_t mode, dev_t rdev)
{
    fuserl_ctx_t* nif_ctx = fuse_req_userdata(req);
    ErlNifEnv* env = nif_ctx->env;
    ERL_NIF_TERM args;

    DEBUGF("op:mknode\r\n", "");

    args = enif_make_list4(env,
			  enif_make_uint64(env, parent),
			  enif_make_string(env, name, ERL_NIF_LATIN1),
			  enif_make_uint64(env,fuserl_stat_mode_canonicalize(mode)),
			  make_dev(env, rdev));
    send_msg(env, &nif_ctx->pid, req, ATOM(mknod), args);
}

/**
 * Create a directory
 *
 * Valid replies:
 *   fuse_reply_entry
 *   fuse_reply_err
 *
 * @param req request handle
 * @param parent inode number of the parent directory
 * @param name to create
 * @param mode with which to create the new file
 */
static void fuse_op_mkdir(fuse_req_t req, fuse_ino_t parent, const char *name,
			  mode_t mode)
{
    fuserl_ctx_t* nif_ctx = fuse_req_userdata(req);
    ErlNifEnv* env = nif_ctx->env;
    ERL_NIF_TERM args;

    DEBUGF("op:mkdir\r\n", "");

    args = enif_make_list3(env,
			  enif_make_uint64(env, parent),
			  enif_make_string(env, name, ERL_NIF_LATIN1),
			  enif_make_uint64(env,fuserl_stat_mode_canonicalize(mode)));
    send_msg(env, &nif_ctx->pid, req, ATOM(mkdir), args);
}

/**
 * Remove a file
 *
 * If the file's inode's lookup count is non-zero, the file
 * system is expected to postpone any removal of the inode
 * until the lookup count reaches zero (see description of the
 * forget function).
 *
 * Valid replies:
 *   fuse_reply_err
 *
 * @param req request handle
 * @param parent inode number of the parent directory
 * @param name to remove
 */
static void fuse_op_unlink(fuse_req_t req, fuse_ino_t parent, const char *name)
{
    fuserl_ctx_t* nif_ctx = fuse_req_userdata(req);
    ErlNifEnv* env = nif_ctx->env;
    ERL_NIF_TERM args;

    DEBUGF("op:unlink\r\n", "");

    args = enif_make_list2(env,
			  enif_make_uint64(env, parent),
			  enif_make_string(env, name, ERL_NIF_LATIN1));
    send_msg(env, &nif_ctx->pid, req, ATOM(unlink), args);    
}


/**
 * Remove a directory
 *
 * If the directory's inode's lookup count is non-zero, the
 * file system is expected to postpone any removal of the
 * inode until the lookup count reaches zero (see description
 * of the forget function).
 *
 * Valid replies:
 *   fuse_reply_err
 *
 * @param req request handle
 * @param parent inode number of the parent directory
 * @param name to remove
 */
static void fuse_op_rmdir(fuse_req_t req, fuse_ino_t parent, const char *name)
{
    fuserl_ctx_t* nif_ctx = fuse_req_userdata(req);
    ErlNifEnv* env = nif_ctx->env;
    ERL_NIF_TERM args;

    DEBUGF("op:rmdir\r\n", "");

    args = enif_make_list2(env,
			  enif_make_uint64(env, parent),
			  enif_make_string(env, name, ERL_NIF_LATIN1));
    send_msg(env, &nif_ctx->pid, req, ATOM(rmdir), args);
}


/**
 * Create a symbolic link
 *
 * Valid replies:
 *   fuse_reply_entry
 *   fuse_reply_err
 *
 * @param req request handle
 * @param link the contents of the symbolic link
 * @param parent inode number of the parent directory
 * @param name to create
 */
static void fuse_op_symlink(fuse_req_t req, const char *link, fuse_ino_t parent,
			    const char *name)
{
    fuserl_ctx_t* nif_ctx = fuse_req_userdata(req);
    ErlNifEnv* env = nif_ctx->env;
    ERL_NIF_TERM args;

    DEBUGF("op:symlink\r\n", "");

    args = enif_make_list3(env,
			  enif_make_string(env, link, ERL_NIF_LATIN1),
			  enif_make_uint64(env, parent),
			  enif_make_string(env, name, ERL_NIF_LATIN1));
    send_msg(env, &nif_ctx->pid, req, ATOM(symlink), args);    
}

/** Rename a file
 *
 * If the target exists it should be atomically replaced. If
 * the target's inode's lookup count is non-zero, the file
 * system is expected to postpone any removal of the inode
 * until the lookup count reaches zero (see description of the
 * forget function).
 *
 * Valid replies:
 *   fuse_reply_err
 *
 * @param req request handle
 * @param parent inode number of the old parent directory
 * @param name old name
 * @param newparent inode number of the new parent directory
 * @param newname new name
 */
static void fuse_op_rename(fuse_req_t req, fuse_ino_t parent, const char *name,
			   fuse_ino_t newparent, const char *newname)
{
    fuserl_ctx_t* nif_ctx = fuse_req_userdata(req);
    ErlNifEnv* env = nif_ctx->env;
    ERL_NIF_TERM args;

    DEBUGF("op:rename\r\n", "");

    args = enif_make_list4(env,
			  enif_make_uint64(env, parent),						  enif_make_string(env, name, ERL_NIF_LATIN1),
			  enif_make_uint64(env, newparent),						  enif_make_string(env, newname, ERL_NIF_LATIN1));
    send_msg(env, &nif_ctx->pid, req, ATOM(rename), args);    
}


/**
 * Create a hard link
 *
 * Valid replies:
 *   fuse_reply_entry
 *   fuse_reply_err
 *
 * @param req request handle
 * @param ino the old inode number
 * @param newparent inode number of the new parent directory
 * @param newname new name to create
 */
static void fuse_op_link(fuse_req_t req, fuse_ino_t ino, fuse_ino_t newparent,
			 const char *newname)
{
    fuserl_ctx_t* nif_ctx = fuse_req_userdata(req);
    ErlNifEnv* env = nif_ctx->env;
    ERL_NIF_TERM args;

    DEBUGF("op:link\r\n", "");

    args = enif_make_list3(env,
			  enif_make_uint64(env, ino),
			  enif_make_uint64(env, newparent),
			  enif_make_string(env, newname, ERL_NIF_LATIN1));
    send_msg(env, &nif_ctx->pid, req, ATOM(link), args);
}


/**
 * Open a file
 *
 * Open flags (with the exception of O_CREAT, O_EXCL, O_NOCTTY and
 * O_TRUNC) are available in fi->flags.
 *
 * Filesystem may store an arbitrary file handle (pointer, index,
 * etc) in fi->fh, and use this in other all other file operations
 * (read, write, flush, release, fsync).
 *
 * Filesystem may also implement stateless file I/O and not store
 * anything in fi->fh.
 *
 * There are also some flags (direct_io, keep_cache) which the
 * filesystem may set in fi, to change the way the file is opened.
 * See fuse_file_info structure in <fuse_common.h> for more details.
 *
 * Valid replies:
 *   fuse_reply_open
 *   fuse_reply_err
 *
 * @param req request handle
 * @param ino the inode number
 * @param fi file information
 */
static void fuse_op_open(fuse_req_t req, fuse_ino_t ino,
			 struct fuse_file_info *fi)
{
    fuserl_ctx_t* nif_ctx = fuse_req_userdata(req);
    ErlNifEnv* env = nif_ctx->env;
    ERL_NIF_TERM args;

    DEBUGF("op:open\r\n", "");

    args = enif_make_list2(env,
			  enif_make_uint64(env, ino),
			  make_fuse_file_info(env, fi));
    send_msg(env, &nif_ctx->pid, req, ATOM(open), args);
}


/**
 * Read data
 *
 * Read should send exactly the number of bytes requested except
 * on EOF or error, otherwise the rest of the data will be
 * substituted with zeroes.  An exception to this is when the file
 * has been opened in 'direct_io' mode, in which case the return
 * value of the read system call will reflect the return value of
 * this operation.
 *
 * fi->fh will contain the value set by the open method, or will
 * be undefined if the open method didn't set any value.
 *
 * Valid replies:
 *   fuse_reply_buf
 *   fuse_reply_iov
 *   fuse_reply_data
 *   fuse_reply_err
 *
 * @param req request handle
 * @param ino the inode number
 * @param size number of bytes to read
 * @param off offset to read from
 * @param fi file information
 */
static void fuse_op_read(fuse_req_t req, fuse_ino_t ino, size_t size, off_t off,
			 struct fuse_file_info *fi)
{
    fuserl_ctx_t* nif_ctx = fuse_req_userdata(req);
    ErlNifEnv* env = nif_ctx->env;
    ERL_NIF_TERM args;

    DEBUGF("op:read\r\n", "");

    args = enif_make_list4(env,
			  enif_make_uint64(env, ino),
			  enif_make_uint64(env, size),
			  enif_make_uint64(env, off),
			  make_fuse_file_info(env, fi));
    send_msg(env, &nif_ctx->pid, req, ATOM(read), args);    
}


/**
 * Write data
 *
 * Write should return exactly the number of bytes requested
 * except on error.  An exception to this is when the file has
 * been opened in 'direct_io' mode, in which case the return value
 * of the write system call will reflect the return value of this
 * operation.
 *
 * fi->fh will contain the value set by the open method, or will
 * be undefined if the open method didn't set any value.
 *
 * Valid replies:
 *   fuse_reply_write
 *   fuse_reply_err
 *
 * @param req request handle
 * @param ino the inode number
 * @param buf data to write
 * @param size number of bytes to write
 * @param off offset to write to
 * @param fi file information
 */
static void fuse_op_write(fuse_req_t req, fuse_ino_t ino, const char *buf,
			  size_t size, off_t off, struct fuse_file_info *fi)
{
    fuserl_ctx_t* nif_ctx = fuse_req_userdata(req);
    ErlNifEnv* env = nif_ctx->env;
    ERL_NIF_TERM args;
    ERL_NIF_TERM data;
    unsigned char* ptr;

    DEBUGF("op:write ctx->buf:%x:%ld buf:%x:%ld \r\n",
	   nif_ctx->buf, fuse_chan_bufsize (nif_ctx->chan),
	   buf, size);

    // FIXME: make data a sub binary from the scan buffer!!!!
    // keep a pool of scan buffers and keep track on number of them
    // in use.
    ptr = enif_make_new_binary(env, size, &data);
    memcpy(ptr, buf, size);

    args = enif_make_list4(env,
			  enif_make_uint64(env, ino),
			  data,			  
			  enif_make_uint64(env, off),
			  make_fuse_file_info(env, fi));
    send_msg(env, &nif_ctx->pid, req, ATOM(write), args);        
}


/**
 * Flush method
 *
 * This is called on each close() of the opened file.
 *
 * Since file descriptors can be duplicated (dup, dup2, fork), for
 * one open call there may be many flush calls.
 *
 * Filesystems shouldn't assume that flush will always be called
 * after some writes, or that if will be called at all.
 *
 * fi->fh will contain the value set by the open method, or will
 * be undefined if the open method didn't set any value.
 *
 * NOTE: the name of the method is misleading, since (unlike
 * fsync) the filesystem is not forced to flush pending writes.
 * One reason to flush data, is if the filesystem wants to return
 * write errors.
 *
 * If the filesystem supports file locking operations (setlk,
 * getlk) it should remove all locks belonging to 'fi->owner'.
 *
 * Valid replies:
 *   fuse_reply_err
 *
 * @param req request handle
 * @param ino the inode number
 * @param fi file information
 */
static void fuse_op_flush(fuse_req_t req, fuse_ino_t ino,
			  struct fuse_file_info *fi)
{
    fuserl_ctx_t* nif_ctx = fuse_req_userdata(req);
    ERL_NIF_TERM args;
    ErlNifEnv* env = nif_ctx->env;

    DEBUGF("op:flush\r\n", "");
    
    args = enif_make_list2(env,
			  enif_make_uint64(env, ino),
			  make_fuse_file_info(env, fi));
    send_msg(env, &nif_ctx->pid, req, ATOM(flush), args);
}


/**
 * Release an open file
 *
 * Release is called when there are no more references to an open
 * file: all file descriptors are closed and all memory mappings
 * are unmapped.
 *
 * For every open call there will be exactly one release call.
 *
 * The filesystem may reply with an error, but error values are
 * not returned to close() or munmap() which triggered the
 * release.
 *
 * fi->fh will contain the value set by the open method, or will
 * be undefined if the open method didn't set any value.
 * fi->flags will contain the same flags as for open.
 *
 * Valid replies:
 *   fuse_reply_err
 *
 * @param req request handle
 * @param ino the inode number
 * @param fi file information
 */
static void fuse_op_release(fuse_req_t req, fuse_ino_t ino,
			    struct fuse_file_info *fi)
{
    fuserl_ctx_t* nif_ctx = fuse_req_userdata(req);
    ERL_NIF_TERM args;
    ErlNifEnv* env = nif_ctx->env;

    DEBUGF("op:release\r\n", "");

    args = enif_make_list2(env,
			  enif_make_uint64(env, ino),
			  make_fuse_file_info(env, fi));
    send_msg(env, &nif_ctx->pid, req, ATOM(release), args);    
}


/**
 * Synchronize file contents
 *
 * If the datasync parameter is non-zero, then only the user data
 * should be flushed, not the meta data.
 *
 * Valid replies:
 *   fuse_reply_err
 *
 * @param req request handle
 * @param ino the inode number
 * @param datasync flag indicating if only data should be flushed
 * @param fi file information
 */
static void fuse_op_fsync(fuse_req_t req, fuse_ino_t ino, int datasync,
			  struct fuse_file_info *fi)
{
    fuserl_ctx_t* nif_ctx = fuse_req_userdata(req);
    ERL_NIF_TERM args;
    ErlNifEnv* env = nif_ctx->env;

    DEBUGF("op:fsync\r\n", "");
    
    args = enif_make_list3(env,
			  enif_make_uint64(env, ino),
			  make_boolean(env, datasync),
			  make_fuse_file_info(env, fi));
    send_msg(env, &nif_ctx->pid, req, ATOM(fsync), args);        
}


/**
 * Open a directory
 *
 * Filesystem may store an arbitrary file handle (pointer, index,
 * etc) in fi->fh, and use this in other all other directory
 * stream operations (readdir, releasedir, fsyncdir).
 *
 * Filesystem may also implement stateless directory I/O and not
 * store anything in fi->fh, though that makes it impossible to
 * implement standard conforming directory stream operations in
 * case the contents of the directory can change between opendir
 * and releasedir.
 *
 * Valid replies:
 *   fuse_reply_open
 *   fuse_reply_err
 *
 * @param req request handle
 * @param ino the inode number
 * @param fi file information
 */
static void fuse_op_opendir(fuse_req_t req, fuse_ino_t ino,
			    struct fuse_file_info *fi)
{
    fuserl_ctx_t* nif_ctx = fuse_req_userdata(req);
    ErlNifEnv* env = nif_ctx->env;
    ERL_NIF_TERM args;

    DEBUGF("op:opendir\r\n", "");
    
    args = enif_make_list2(env,
			  enif_make_uint64(env, ino),
			  make_fuse_file_info(env, fi));
    send_msg(env, &nif_ctx->pid, req, ATOM(opendir), args);    
}


/**
 * Read directory
 *
 * Send a buffer filled using fuse_add_direntry(), with size not
 * exceeding the requested size.  Send an empty buffer on end of
 * stream.
 *
 * fi->fh will contain the value set by the opendir method, or
 * will be undefined if the opendir method didn't set any value.
 *
 * Valid replies:
 *   fuse_reply_buf
 *   fuse_reply_data
 *   fuse_reply_err
 *
 * @param req request handle
 * @param ino the inode number
 * @param size maximum number of bytes to send
 * @param off offset to continue reading the directory stream
 * @param fi file information
 */
static void fuse_op_readdir(fuse_req_t req, fuse_ino_t ino, size_t size,
			    off_t off, struct fuse_file_info *fi)
{
    fuserl_ctx_t* nif_ctx = fuse_req_userdata(req);
    ErlNifEnv* env = nif_ctx->env;
    ERL_NIF_TERM args;

    DEBUGF("op:readdir\r\n", "");

    args = enif_make_list4(env,
			  enif_make_uint64(env, ino),
			  enif_make_uint64(env, size),
			  enif_make_uint64(env, off),
			  make_fuse_file_info(env, fi));
    send_msg(env, &nif_ctx->pid, req, ATOM(readdir), args);        
}


/**
 * Release an open directory
 *
 * For every opendir call there will be exactly one releasedir
 * call.
 *
 * fi->fh will contain the value set by the opendir method, or
 * will be undefined if the opendir method didn't set any value.
 *
 * Valid replies:
 *   fuse_reply_err
 *
 * @param req request handle
 * @param ino the inode number
 * @param fi file information
 */
static void fuse_op_releasedir(fuse_req_t req, fuse_ino_t ino,
		    struct fuse_file_info *fi)
{
    fuserl_ctx_t* nif_ctx = fuse_req_userdata(req);
    ErlNifEnv* env = nif_ctx->env;
    ERL_NIF_TERM args;

    DEBUGF("op:releasedir\r\n", "");

    args = enif_make_list2(env,
			  enif_make_uint64(env, ino),
			  make_fuse_file_info(env, fi));
    send_msg(env, &nif_ctx->pid, req, ATOM(releasedir), args);        
}


/**
 * Synchronize directory contents
 *
 * If the datasync parameter is non-zero, then only the directory
 * contents should be flushed, not the meta data.
 *
 * fi->fh will contain the value set by the opendir method, or
 * will be undefined if the opendir method didn't set any value.
 *
 * Valid replies:
 *   fuse_reply_err
 *
 * @param req request handle
 * @param ino the inode number
 * @param datasync flag indicating if only data should be flushed
 * @param fi file information
 */
static void fuse_op_fsyncdir(fuse_req_t req, fuse_ino_t ino, int datasync,
		  struct fuse_file_info *fi)
{
    fuserl_ctx_t* nif_ctx = fuse_req_userdata(req);
    ErlNifEnv* env = nif_ctx->env;
    ERL_NIF_TERM args;

    DEBUGF("op:fsyncdir\r\n", "");

    args = enif_make_list3(env,
			  enif_make_uint64(env, ino),
			  make_boolean(env, datasync),
			  make_fuse_file_info(env, fi));
    send_msg(env, &nif_ctx->pid, req, ATOM(fsyncdir), args);
}


/**
 * Get file system statistics
 *
 * Valid replies:
 *   fuse_reply_statfs
 *   fuse_reply_err
 *
 * @param req request handle
 * @param ino the inode number, zero means "undefined"
 */
static void fuse_op_statfs(fuse_req_t req, fuse_ino_t ino)
{
    fuserl_ctx_t* nif_ctx = fuse_req_userdata(req);
    ErlNifEnv* env = nif_ctx->env;
    ERL_NIF_TERM args;

    DEBUGF("op:statfs\r\n", "");

    args = enif_make_list1(env,
			  enif_make_uint64(env, ino));
    send_msg(env, &nif_ctx->pid, req, ATOM(statfs), args);    
}


/**
 * Set an extended attribute
 *
 * Valid replies:
 *   fuse_reply_err
 */
static void fuse_op_setxattr(fuse_req_t req, fuse_ino_t ino, const char *name,
		  const char *value, size_t size, int flags)
{
    fuserl_ctx_t* nif_ctx = fuse_req_userdata(req);
    ErlNifEnv* env = nif_ctx->env;
    ERL_NIF_TERM args;

    DEBUGF("op:setxattr\r\n", "");

    args = enif_make_list5(env,
			  enif_make_uint64(env, ino),
			  enif_make_string(env, name, ERL_NIF_LATIN1),
			  enif_make_string(env, value, ERL_NIF_LATIN1),
			  enif_make_uint64(env, size),
			  enif_make_int(env, flags));
    send_msg(env, &nif_ctx->pid, req, ATOM(setxattr), args);  
}

/**
 * Get an extended attribute
 *
 * If size is zero, the size of the value should be sent with
 * fuse_reply_xattr.
 *
 * If the size is non-zero, and the value fits in the buffer, the
 * value should be sent with fuse_reply_buf.
 *
 * If the size is too small for the value, the ERANGE error should
 * be sent.
 *
 * Valid replies:
 *   fuse_reply_buf
 *   fuse_reply_data
 *   fuse_reply_xattr
 *   fuse_reply_err
 *
 * @param req request handle
 * @param ino the inode number
 * @param name of the extended attribute
 * @param size maximum size of the value to send
 */
static void fuse_op_getxattr(fuse_req_t req, fuse_ino_t ino, const char *name,
		  size_t size)
{
    fuserl_ctx_t* nif_ctx = fuse_req_userdata(req);
    ErlNifEnv* env = nif_ctx->env;
    ERL_NIF_TERM args;

    DEBUGF("op:getxattr\r\n", "");

    args = enif_make_list3(env,
			  enif_make_uint64(env, ino),
			  enif_make_string(env, name, ERL_NIF_LATIN1),
			  enif_make_uint64(env, size));
    send_msg(env, &nif_ctx->pid, req, ATOM(getxattr), args);        
}


/**
 * List extended attribute names
 *
 * If size is zero, the total size of the attribute list should be
 * sent with fuse_reply_xattr.
 *
 * If the size is non-zero, and the null character separated
 * attribute list fits in the buffer, the list should be sent with
 * fuse_reply_buf.
 *
 * If the size is too small for the list, the ERANGE error should
 * be sent.
 *
 * Valid replies:
 *   fuse_reply_buf
 *   fuse_reply_data
 *   fuse_reply_xattr
 *   fuse_reply_err
 *
 * @param req request handle
 * @param ino the inode number
 * @param size maximum size of the list to send
 */
static void fuse_op_listxattr(fuse_req_t req, fuse_ino_t ino, size_t size)
{
    fuserl_ctx_t* nif_ctx = fuse_req_userdata(req);
    ErlNifEnv* env = nif_ctx->env;
    ERL_NIF_TERM args;

    DEBUGF("op:listxattr\r\n", "");

    args = enif_make_list2(env,
			  enif_make_uint64(env, ino),
			  enif_make_uint64(env, size));
    send_msg(env, &nif_ctx->pid, req, ATOM(listxattr), args);
}


/**
 * Remove an extended attribute
 *
 * Valid replies:
 *   fuse_reply_err
 *
 * @param req request handle
 * @param ino the inode number
 * @param name of the extended attribute
 */
static void fuse_op_removexattr(fuse_req_t req, fuse_ino_t ino, const char *name)
{
    fuserl_ctx_t* nif_ctx = fuse_req_userdata(req);
    ErlNifEnv* env = nif_ctx->env;
    ERL_NIF_TERM args;

    DEBUGF("op:removexattr\r\n", "");

    args = enif_make_list2(env,
			  enif_make_uint64(env, ino),
			  enif_make_string(env, name, ERL_NIF_LATIN1));
    send_msg(env, &nif_ctx->pid, req, ATOM(removexattr), args);
}


/**
 * Check file access permissions
 *
 * This will be called for the access() system call.  If the
 * 'default_permissions' mount option is given, this method is not
 * called.
 *
 * This method is not called under Linux kernel versions 2.4.x
 *
 * Introduced in version 2.5
 *
 * Valid replies:
 *   fuse_reply_err
 *
 * @param req request handle
 * @param ino the inode number
 * @param mask requested access mode
 */
static void fuse_op_access(fuse_req_t req, fuse_ino_t ino, int mask)
{
    fuserl_ctx_t* nif_ctx = fuse_req_userdata(req);
    ErlNifEnv* env = nif_ctx->env;
    ERL_NIF_TERM args;

    DEBUGF("op:access\r\n", "");

    args = enif_make_list2(env,
			  enif_make_uint64(env, ino),
			  enif_make_int(env, fuserl_access_mode_canonicalize(mask)));
    send_msg(env, &nif_ctx->pid, req, ATOM(access), args);
}


/**
 * Create and open a file
 *
 * If the file does not exist, first create it with the specified
 * mode, and then open it.
 *
 * Open flags (with the exception of O_NOCTTY) are available in
 * fi->flags.
 *
 * Filesystem may store an arbitrary file handle (pointer, index,
 * etc) in fi->fh, and use this in other all other file operations
 * (read, write, flush, release, fsync).
 *
 * There are also some flags (direct_io, keep_cache) which the
 * filesystem may set in fi, to change the way the file is opened.
 * See fuse_file_info structure in <fuse_common.h> for more details.
 *
 * If this method is not implemented or under Linux kernel
 * versions earlier than 2.6.15, the mknod() and open() methods
 * will be called instead.
 *
 * Introduced in version 2.5
 *
 * Valid replies:
 *   fuse_reply_create
 *   fuse_reply_err
 *
 * @param req request handle
 * @param parent inode number of the parent directory
 * @param name to create
 * @param mode file type and mode with which to create the new file
 * @param fi file information
 */
static void fuse_op_create(fuse_req_t req, fuse_ino_t parent, const char *name,
			   mode_t mode, struct fuse_file_info *fi)
{
    fuserl_ctx_t* nif_ctx = fuse_req_userdata(req);
    ErlNifEnv* env = nif_ctx->env;
    ERL_NIF_TERM args;

    DEBUGF("op:create\r\n", "");

    args = enif_make_list4(env,
			  enif_make_uint64(env, parent),
			  enif_make_string(env, name, ERL_NIF_LATIN1),
			  enif_make_uint64(env, fuserl_stat_mode_canonicalize(mode)),
			  make_fuse_file_info(env, fi));
    send_msg(env, &nif_ctx->pid, req, ATOM(create), args);    
}


/**
 * Test for a POSIX file lock
 *
 * Introduced in version 2.6
 *
 * Valid replies:
 *   fuse_reply_lock
 *   fuse_reply_err
 *
 * @param req request handle
 * @param ino the inode number
 * @param fi file information
 * @param lock the region/type to test
 */
static void fuse_op_getlk(fuse_req_t req, fuse_ino_t ino,
			  struct fuse_file_info *fi, struct flock *lock)
{
    fuserl_ctx_t* nif_ctx = fuse_req_userdata(req);
    ErlNifEnv* env = nif_ctx->env;
    ERL_NIF_TERM args;

    DEBUGF("op:getlkr\n", "");

    args = enif_make_list3(env,
			  enif_make_uint64(env, ino),
			  make_fuse_file_info(env, fi),
			  make_flock(env, lock));
    send_msg(env, &nif_ctx->pid, req, ATOM(getlk), args);
}


/**
 * Acquire, modify or release a POSIX file lock
 *
 * For POSIX threads (NPTL) there's a 1-1 relation between pid and
 * owner, but otherwise this is not always the case.  For checking
 * lock ownership, 'fi->owner' must be used.  The l_pid field in
 * 'struct flock' should only be used to fill in this field in
 * getlk().
 *
 * Note: if the locking methods are not implemented, the kernel
 * will still allow file locking to work locally.  Hence these are
 * only interesting for network filesystems and similar.
 *
 * Introduced in version 2.6
 *
 * Valid replies:
 *   fuse_reply_err
 *
 * @param req request handle
 * @param ino the inode number
 * @param fi file information
 * @param lock the region/type to set
 * @param sleep locking operation may sleep
 */
static void fuse_op_setlk(fuse_req_t req, fuse_ino_t ino,
			  struct fuse_file_info *fi,
			  struct flock *lock, int sleep)
{
    fuserl_ctx_t* nif_ctx = fuse_req_userdata(req);
    ErlNifEnv* env = nif_ctx->env;
    ERL_NIF_TERM args;

    DEBUGF("op:setlkr\n", "");

    args = enif_make_list4(env,
			  enif_make_uint64(env, ino),
			  make_fuse_file_info(env, fi),
			  make_flock(env, lock),
			  make_boolean(env, sleep));
    send_msg(env, &nif_ctx->pid, req, ATOM(setlk), args);    
}


/**
 * Map block index within file to block index within device
 *
 * Note: This makes sense only for block device backed filesystems
 * mounted with the 'blkdev' option
 *
 * Introduced in version 2.6
 *
 * Valid replies:
 *   fuse_reply_bmap
 *   fuse_reply_err
 *
 * @param req request handle
 * @param ino the inode number
 * @param blocksize unit of block index
 * @param idx block index within file
 */
static void fuse_op_bmap(fuse_req_t req, fuse_ino_t ino, size_t blocksize,
			 uint64_t idx)
{
    fuserl_ctx_t* nif_ctx = fuse_req_userdata(req);
    ErlNifEnv* env = nif_ctx->env;
    ERL_NIF_TERM args;

    DEBUGF("op:bmap\r\n", "");
    
    args = enif_make_list3(env,
			  enif_make_uint64(env, ino),
			  enif_make_uint64(env, blocksize),
			  enif_make_uint64(env, idx));
    send_msg(env, &nif_ctx->pid, req, ATOM(bmap), args);        
}


/**
 * Ioctl
 *
 * Note: For unrestricted ioctls (not allowed for FUSE
 * servers), data in and out areas can be discovered by giving
 * iovs and setting FUSE_IOCTL_RETRY in @flags.  For
 * restricted ioctls, kernel prepares in/out data area
 * according to the information encoded in cmd.
 *
 * Introduced in version 2.8
 *
 * Valid replies:
 *   fuse_reply_ioctl_retry
 *   fuse_reply_ioctl
 *   fuse_reply_ioctl_iov
 *   fuse_reply_err
 *
 * @param req request handle
 * @param ino the inode number
 * @param cmd ioctl command
 * @param arg ioctl argument
 * @param fi file information
 * @param flags for FUSE_IOCTL_* flags
 * @param in_buf data fetched from the caller
 * @param in_bufsz number of fetched bytes
 * @param out_bufsz maximum size of output data
 */
static void fuse_op_ioctl(fuse_req_t req, fuse_ino_t ino, int cmd, void *arg,
			  struct fuse_file_info *fi, unsigned flags,
			  const void *in_buf, size_t in_bufsz, size_t out_bufsz)
{
    fuserl_ctx_t* nif_ctx = fuse_req_userdata(req);
    ErlNifEnv* env = nif_ctx->env;
    ERL_NIF_TERM args, in_data;
    unsigned char* ptr;

    DEBUGF("op:ioctl\r\n", "");
    
    ptr = enif_make_new_binary(env, in_bufsz, &in_data);
    memcpy(ptr, in_buf, in_bufsz);    

    args = enif_make_list7(env,
			  enif_make_uint64(env, ino),
			  enif_make_int(env, cmd),
			  make_pointer(env, arg),  // nope!?
			  make_fuse_file_info(env, fi),
			  enif_make_uint(env, flags),
			  in_data,  // size is in the binary arg
			  enif_make_uint64(env, out_bufsz));
    send_msg(env, &nif_ctx->pid, req, ATOM(ioctl), args);        
    
}


/**
 * Poll for IO readiness
 *
 * Introduced in version 2.8
 *
 * Note: If ph is non-NULL, the client should notify
 * when IO readiness events occur by calling
 * fuse_lowlevel_notify_poll() with the specified ph.
 *
 * Regardless of the number of times poll with a non-NULL ph
 * is received, single notification is enough to clear all.
 * Notifying more times incurs overhead but doesn't harm
 * correctness.
 *
 * The callee is responsible for destroying ph with
 * fuse_pollhandle_destroy() when no longer in use.
 *
 * Valid replies:
 *   fuse_reply_poll
 *   fuse_reply_err
 *
 * @param req request handle
 * @param ino the inode number
 * @param fi file information
 * @param ph poll handle to be used for notification
 */
static void fuse_op_poll(fuse_req_t req, fuse_ino_t ino,
			 struct fuse_file_info *fi,
			 struct fuse_pollhandle *ph)
{
    DEBUGF("op:poll\r\n", "");
}


/**
 * Write data made available in a buffer
 *
 * This is a more generic version of the ->write() method.  If
 * FUSE_CAP_SPLICE_READ is set in fuse_conn_info.want and the
 * kernel supports splicing from the fuse device, then the
 * data will be made available in pipe for supporting zero
 * copy data transfer.
 *
 * buf->count is guaranteed to be one (and thus buf->idx is
 * always zero). The write_buf handler must ensure that
 * bufv->off is correctly updated (reflecting the number of
 * bytes read from bufv->buf[0]).
 *
 * Introduced in version 2.9
 *
 * Valid replies:
 *   fuse_reply_write
 *   fuse_reply_err
 *
 * @param req request handle
 * @param ino the inode number
 * @param bufv buffer containing the data
 * @param off offset to write to
 * @param fi file information
 */
static void fuse_op_write_buf(fuse_req_t req, fuse_ino_t ino,
			      struct fuse_bufvec *bufv, off_t off,
			      struct fuse_file_info *fi)
{
    DEBUGF("op:write_buf\r\n", "");
}

/**
 * Callback function for the retrieve request
 *
 * Introduced in version 2.9
 *
 * Valid replies:
 *	fuse_reply_none
 *
 * @param req request handle
 * @param cookie user data supplied to fuse_lowlevel_notify_retrieve()
 * @param ino the inode number supplied to fuse_lowlevel_notify_retrieve()
 * @param offset the offset supplied to fuse_lowlevel_notify_retrieve()
 * @param bufv the buffer containing the returned data
 */
static void fuse_op_retrieve_reply(fuse_req_t req, void *cookie, fuse_ino_t ino,
				   off_t offset, struct fuse_bufvec *bufv)
{
    DEBUGF("op:retrieve_reply\r\n", "");
}


/**
 * Forget about multiple inodes
 *
 * See description of the forget function for more
 * information.
 *
 * Introduced in version 2.9
 *
 * Valid replies:
 *   fuse_reply_none
 *
 * @param req request handle
 */
static void fuse_op_forget_multi(fuse_req_t req, size_t count,
				 struct fuse_forget_data *forgets)
{
    fuserl_ctx_t* nif_ctx = fuse_req_userdata(req);
    ErlNifEnv* env = nif_ctx->env;
    ERL_NIF_TERM list;
    ERL_NIF_TERM args;
    int i;

    DEBUGF("op:forget_multi\r\n", "");
    
    list = enif_make_list(env, 0);
    for (i = count-1; i >= 0; i--) {
	ERL_NIF_TERM rec =
	    enif_make_tuple3(env,
			     ATOM(fuse_forget_data),
			     enif_make_uint64(env, forgets[i].ino),
			     enif_make_uint64(env, forgets[i].nlookup));
	list = enif_make_list_cell(env, rec, list);
    }
    args = enif_make_list2(env,
			   enif_make_uint64(env, count),
			   list),
    send_msg(env, &nif_ctx->pid, req, ATOM(forget_multi), args);
}


/**
 * Acquire, modify or release a BSD file lock
 *
 * Note: if the locking methods are not implemented, the kernel
 * will still allow file locking to work locally.  Hence these are
 * only interesting for network filesystems and similar.
 *
 * Introduced in version 2.9
 *
 * Valid replies:
 *   fuse_reply_err
 *
 * @param req request handle
 * @param ino the inode number
 * @param fi file information
 * @param op the locking operation, see flock(2)
 */
static void fuse_op_flock(fuse_req_t req, fuse_ino_t ino,
			  struct fuse_file_info *fi, int op)
{
    fuserl_ctx_t* nif_ctx = fuse_req_userdata(req);
    ErlNifEnv* env = nif_ctx->env;
    ERL_NIF_TERM args;

    DEBUGF("op:flock\r\n", "");
    
    args = enif_make_list3(env,
			   enif_make_uint64(env, ino),
			   make_fuse_file_info(env, fi),
			   enif_make_int(env, op));
    send_msg(env, &nif_ctx->pid, req, ATOM(flock), args);     
}


/**
 * Allocate requested space. If this function returns success then
 * subsequent writes to the specified range shall not fail due to the lack
 * of free space on the file system storage media.
 *
 * Introduced in version 2.9
 *
 * Valid replies:
 *   fuse_reply_err
 *
 * @param req request handle
 * @param ino the inode number
 * @param offset starting point for allocated region
 * @param length size of allocated region
 * @param mode determines the operation to be performed on the given range,
 *             see fallocate(2)
 */
static void fuse_op_fallocate(fuse_req_t req, fuse_ino_t ino, int mode,
			      off_t offset, off_t length,
			      struct fuse_file_info *fi)
{
    fuserl_ctx_t* nif_ctx = fuse_req_userdata(req);
    ErlNifEnv* env = nif_ctx->env;
    ERL_NIF_TERM args;
    int m = fuserl_stat_mode_decanonicalize(mode);

    DEBUGF("op:fallocate\r\n", "");
    
    args = enif_make_list5(env,
			   enif_make_uint64(env, ino),
			   enif_make_int(env,m),
			   enif_make_uint64(env, offset),
			   enif_make_uint64(env, length),
			   make_fuse_file_info(env, fi));

    send_msg(env, &nif_ctx->pid, req, ATOM(fallocate), args); 
}


//////////////////////////////////////////////////////////////////////////////
/// NIFs 
//////////////////////////////////////////////////////////////////////////////

#define ADD_OP(name) do {		       \
    if (atom_member(env,atm_##name,argv[2])) { \
	(ctx)->op.name = fuse_op_##name;       \
	enif_fprintf(stdout, "add_op: %s\r\n", #name);	\
    }							\
    } while(0)

static int set_nonblock(int fd)
{
    int flags = fcntl(fd, F_GETFL, 0);
    return fcntl(fd, F_SETFL, flags|O_NONBLOCK);
}

// mount(MountOpts::string(), MountPoint::string(), OpList::[atom()]) -> Handle
static ERL_NIF_TERM nif_fuserl_mount(ErlNifEnv* env, int argc,
				     const ERL_NIF_TERM argv[])
{
    fuserl_ctx_t* ctx;
    struct fuse_args margs = FUSE_ARGS_INIT (0, NULL);
    ERL_NIF_TERM ret;
    
    if ((ctx = enif_alloc_resource(fuserl_r, sizeof(fuserl_ctx_t))) == NULL) {
	int e = errno;
	return make_error(env, e);
    }
    memset(ctx, 0, sizeof(fuserl_ctx_t));

    enif_self(env, &ctx->pid);    
    
    if (!get_string(env, argv[0], &ctx->mountpoint)) goto error;
    if (!get_string(env, argv[1], &ctx->mountopts)) goto error;
    
    if (fuse_opt_add_arg (&margs, "") == -1) goto error;
    if (fuse_opt_add_arg (&margs, "-o") == -1) goto error;
    if (fuse_opt_add_arg (&margs, ctx->mountopts) == -1) goto error;

    if ((ctx->chan = fuse_mount(ctx->mountpoint, &margs)) == NULL) goto error;
    // fixme: buf should be a resource binary buffer (allow sub binaries!)
    if ((ctx->buf = enif_alloc(fuse_chan_bufsize (ctx->chan))) == NULL)
	goto error;
    // fixme ADD_OP use env and argv[2]
    ADD_OP(init); ADD_OP(destroy);  ADD_OP(lookup); ADD_OP(forget);
    ADD_OP(getattr); ADD_OP(setattr); ADD_OP(readlink); ADD_OP(mknod);
    ADD_OP(mkdir); ADD_OP(unlink); ADD_OP(rmdir); ADD_OP(symlink);
    ADD_OP(rename); ADD_OP(link); ADD_OP(open); ADD_OP(read);
    ADD_OP(write); ADD_OP(flush); ADD_OP(release); ADD_OP(fsync);
    ADD_OP(opendir); ADD_OP(readdir); ADD_OP(releasedir);
    ADD_OP(fsyncdir); ADD_OP(statfs); ADD_OP(setxattr);
    ADD_OP(getxattr); ADD_OP(listxattr); ADD_OP(removexattr);
    ADD_OP(access); ADD_OP(create);  ADD_OP(getlk); ADD_OP(setlk);
    ADD_OP(bmap); ADD_OP(ioctl);  ADD_OP(poll); ADD_OP(write_buf);
    ADD_OP(retrieve_reply); ADD_OP(forget_multi);
    ADD_OP(flock); ADD_OP(fallocate);

    ctx->env = enif_alloc_env();    
    ctx->se = fuse_lowlevel_new(&margs, &ctx->op, sizeof(ctx->op), ctx);
    if (ctx->se == NULL) goto error;
    fuse_session_add_chan(ctx->se, ctx->chan);

    set_nonblock(fuse_chan_fd(ctx->chan));
    
    fuse_opt_free_args (&margs);
    ret = enif_make_resource(env, ctx);
    enif_release_resource(ctx);
    return ret;
error:
    fuse_opt_free_args (&margs);
// error: fixme cleanup ctx
    return enif_make_badarg(env);
}

static ERL_NIF_TERM nif_fuserl_unmount(ErlNifEnv* env, int argc,
				       const ERL_NIF_TERM argv[])
{
    fuserl_ctx_t* ctx;
    
    if (!enif_get_resource(env, argv[0], fuserl_r, (void**)&ctx))
	return enif_make_badarg(env);
    enif_self(env, &ctx->pid);
    return ATOM(ok);
}

static ERL_NIF_TERM nif_fuserl_process(ErlNifEnv* env, int argc,
				       const ERL_NIF_TERM argv[])
{
    fuserl_ctx_t* ctx;
    size_t size;
    char *buf;
    ssize_t n;
    
    if (!enif_get_resource(env, argv[0], fuserl_r, (void**)&ctx))
	return enif_make_badarg(env);

    size = fuse_chan_bufsize(ctx->chan);
    // fixme: use resource binary to be able handle sub binaries
    buf = ctx->buf;

    enif_self(env, &ctx->pid);

    if ((n = read(fuse_chan_fd(ctx->chan), buf, size)) < 0) {
	if ((errno == EAGAIN)||(errno == EINTR)) {
	    enif_select(env, (ErlNifEvent)((long)fuse_chan_fd(ctx->chan)),
			ERL_NIF_SELECT_READ,
			ctx, &ctx->pid, ATOM(undefined));
	    return ATOM(select);
	}
	return make_error(env, errno);
    }
    else if (n == 0) {
	return ATOM(eof);
    }
    DEBUGF("process %ld bytes\r\n", n);
    fuse_session_process (ctx->se, buf, n, ctx->chan);
    return ATOM(ok);
}

// reply(Handle, Req, #reply_xxx{}) -> ok.
static ERL_NIF_TERM nif_fuserl_reply(ErlNifEnv* env, int argc,
				     const ERL_NIF_TERM argv[])
{
    fuserl_ctx_t* ctx;
    int arity;
    const ERL_NIF_TERM* elem;
    fuse_req_t req;
    int r;
    void* ptr;
    
    if (!enif_get_resource(env, argv[0], fuserl_r, (void**)&ctx))
	return enif_make_badarg(env);
    if (!get_pointer(env, argv[1], &ptr))
	return enif_make_badarg(env);
    req = (fuse_req_t) ptr;
	
    if (!enif_get_tuple(env, argv[2], &arity, &elem)) goto error;
    if (!enif_is_atom(env, elem[0])) goto error;
    
    if ((r = lookup_atom(reply_hash, REPLY_HASH_SIZE, elem[0])) < 0)
	goto error;
    switch(r) {
    case FUSE_REPLY_ERR: {
	int err;
	if (!enif_get_int(env, elem[1], &err)) goto error;
	fuse_reply_err (req, fuserl_errno_decanonicalize (err));
	break;
    }
    case FUSE_REPLY_NONE:
	break;
    case FUSE_REPLY_ENTRY: {
	struct fuse_entry_param param;
	if (!get_fuse_entry_param(env, elem[1], &param)) goto error;
	fuse_reply_entry (req, &param);
	break;
    }

    case FUSE_REPLY_CREATE: {
	struct fuse_entry_param param;
	struct fuse_file_info info;
	
	if (!get_fuse_entry_param(env, elem[1], &param)) goto error;
	if (!get_fuse_file_info(env, elem[2], &info)) goto error;
        fuse_reply_create(req, &param, &info);
	break;
    }
	
    case FUSE_REPLY_ATTR: {
	struct stat attr;
	double tm;

	DEBUGF("reply_attr: arity=%d\r\n", arity);

	if (!get_stat(env, elem[1], &attr)) goto error;
	if (!get_timeout(env, elem[2], &tm)) goto error;
	fuse_reply_attr(req, &attr, tm);
	break;
    }
    case FUSE_REPLY_READLINK: {
	char* path;
	if (!get_string(env, elem[1], &path)) goto error;
        fuse_reply_readlink(req, path);
	enif_free(path); // fixme!
	break;
    }

    case FUSE_REPLY_OPEN: {
	struct fuse_file_info info;
	if (!get_fuse_file_info(env, elem[1], &info))  goto error;
	fuse_reply_open(req, &info);
	break;
    }
    case FUSE_REPLY_WRITE: {
	uint64_t count;
	if (!enif_get_uint64(env, elem[1], &count)) goto error;
	fuse_reply_write(req, count);
	break;
    }
    case FUSE_REPLY_BUF: {
	ErlNifBinary bin;
	if (!enif_inspect_iolist_as_binary(env, elem[1], &bin)) goto error;
	fuse_reply_buf(req, (char*)bin.data, bin.size);
	break;
    }

    /* FUSE_REPLY_IOV = 9,  */
    case FUSE_REPLY_STATFS: {
	struct statvfs stbuf;
	if (!get_statvfs(env, elem[1], &stbuf)) goto error;
	fuse_reply_statfs (req, &stbuf);
	break;
    }
#if HAVE_SETXATTR
    case FUSE_REPLY_XATTR: {
	uint64_t count;
	if (!enif_get_uint64(env, elem[1], &count)) goto error;
	fuse_reply_xattr (req, count);
	break;	
    }
#endif
    case FUSE_REPLY_LOCK: {
	struct flock lock;
	if (!get_flock(env, elem[1], &lock)) goto error;
	fuse_reply_lock(req, &lock);
	break;
    }
    /* case FUSE_REPLY_BMAP: */
	
    case FUSE_REPLY_DIRENTRYLIST: {
	char* buf = enif_alloc(8192);
	size_t buf_size = 0;
	size_t buf_max = 8192;
	ERL_NIF_TERM head, tail;
	ERL_NIF_TERM list = elem[1];
	// [ direntry ]
	while(enif_get_list_cell(env, list, &head, &tail)) {
	    int arity;
	    const ERL_NIF_TERM* elem;
	    unsigned int len;
	    
	    if (!enif_get_tuple(env, head, &arity, &elem) || (arity != 4))
		return 0;
	    if (elem[0] != ATOM(direntry)) goto dir_error;
	    if (!enif_get_list_length(env, elem[1], &len)) goto dir_error;
	    else {
		char name[len+1];
		uint64_t u64;
		off_t offset;
		struct stat stbuf;
		size_t incr;
		
		if (!enif_get_string(env, elem[1], name, len+1, ERL_NIF_LATIN1))
		    goto dir_error;
		if (!enif_get_uint64(env, elem[2], &u64)) goto dir_error;
		offset = (off_t)u64;
		if (!get_stat(env, elem[3], &stbuf)) goto dir_error;
		
		incr = fuse_add_direntry (req, NULL, 0, name, NULL, 0);
		if (buf_size + incr > buf_max) {
		    char* nb = enif_realloc(buf, buf_max + MAX(buf_max,incr));
		    if (nb == NULL) goto dir_error;
		    buf = nb;
		    buf_max += MAX (buf_max,incr);
		}
		fuse_add_direntry (req, buf+buf_size, buf_max-buf_size, name,
				   &stbuf, offset);
		buf_size += incr;
	    }
	    list = tail;
	}

	fuse_reply_buf(req, buf, buf_size);
	enif_free(buf);
	return ATOM(ok);
	dir_error:
	enif_free(buf);
	return enif_make_badarg(env);
    }
	// fuse_reply_direntrylist, { direntrylist }).
    default:
	return enif_make_badarg(env);
    }
    return ATOM(ok);
error:
    return enif_make_badarg(env);
}

// create all tracing NIFs
#ifdef NIF_TRACE

#undef NIF

static void trace_print_arg_list(ErlNifEnv* env,int argc,const ERL_NIF_TERM argv[])
{
    enif_fprintf(stdout, "(");
    if (argc > 0) {
	int i;
	if (enif_is_ref(env, argv[0])) {
	    // FIXME print object type if available
	    enif_fprintf(stdout, "%T", argv[0]);
	}
	else
	    enif_fprintf(stdout, "%T", argv[0]);
	for (i = 1; i < argc; i++)
	    enif_fprintf(stdout, ",%T", argv[i]);
    }
    enif_fprintf(stdout, ")");
}

#define NIF(name, arity, func)					\
static ERL_NIF_TERM trace##_##func##_##arity(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]) \
{ \
    ERL_NIF_TERM result;					\
    enif_fprintf(stdout, "ENTER %s", (name));			\
    trace_print_arg_list(env, argc, argv);			\
    enif_fprintf(stdout, "\r\n");				\
    result = func(env, argc, argv);				\
    enif_fprintf(stdout, "  RESULT=%T\r\n", (result));		\
    enif_fprintf(stdout, "LEAVE %s\r\n", (name));		\
    return result;						\
}

NIF_LIST

#endif

static int load_atoms(ErlNifEnv* env)
{
    LOAD_ATOM(ok);
    LOAD_ATOM(error);
    LOAD_ATOM(undefined);
    LOAD_ATOM(select);
    LOAD_ATOM(eof);
    LOAD_ATOM(true);
    LOAD_ATOM(false);    
    // record type names
    LOAD_ATOM(fuserl);
    LOAD_ATOM(fuse_file_info);
    LOAD_ATOM(fuse_entry_param);    
    LOAD_ATOM(fuse_ctx);    
    LOAD_ATOM(stat);
    LOAD_ATOM(statvfs);
    LOAD_ATOM(flock);
    LOAD_ATOM(direntry);
    LOAD_ATOM(fuse_forget_data);    
    // operations names
    LOAD_ATOM(init);
    LOAD_ATOM(destroy);
    LOAD_ATOM(lookup);
    LOAD_ATOM(forget);
    LOAD_ATOM(getattr);
    LOAD_ATOM(setattr);
    LOAD_ATOM(readlink);
    LOAD_ATOM(mknod);
    LOAD_ATOM(mkdir);
    LOAD_ATOM(unlink);
    LOAD_ATOM(rmdir);
    LOAD_ATOM(symlink);
    LOAD_ATOM(rename);
    LOAD_ATOM(link);
    LOAD_ATOM(open);
    LOAD_ATOM(read);
    LOAD_ATOM(write);
    LOAD_ATOM(flush);
    LOAD_ATOM(release);
    LOAD_ATOM(fsync);
    LOAD_ATOM(opendir);
    LOAD_ATOM(readdir);
    LOAD_ATOM(releasedir);
    LOAD_ATOM(fsyncdir);
    LOAD_ATOM(statfs);
    LOAD_ATOM(setxattr);
    LOAD_ATOM(getxattr);
    LOAD_ATOM(listxattr);
    LOAD_ATOM(removexattr);
    LOAD_ATOM(access);
    LOAD_ATOM(create);
    LOAD_ATOM(getlk);
    LOAD_ATOM(setlk);
    LOAD_ATOM(bmap);
    LOAD_ATOM(ioctl);
    LOAD_ATOM(poll);
    LOAD_ATOM(write_buf);
    LOAD_ATOM(retrieve_reply);
    LOAD_ATOM(forget_multi);
    // LOAD_ATOM(flock);
    LOAD_ATOM(fallocate);
    // reply
    LOAD_ATOM(fuse_reply_err);
    LOAD_ATOM(fuse_reply_none);
    LOAD_ATOM(fuse_reply_entry);
    LOAD_ATOM(fuse_reply_create);
    LOAD_ATOM(fuse_reply_attr);
    LOAD_ATOM(fuse_reply_readlink);
    LOAD_ATOM(fuse_reply_open);
    LOAD_ATOM(fuse_reply_write);
    LOAD_ATOM(fuse_reply_buf);
    LOAD_ATOM(fuse_reply_statfs);
    LOAD_ATOM(fuse_reply_xattr);
    LOAD_ATOM(fuse_reply_lock);
    LOAD_ATOM(fuse_reply_direntrylist);    
    return 0;
}

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    UNUSED(env);
    UNUSED(load_info);
    ErlNifResourceFlags tried;
    ErlNifResourceTypeInit cb;
    struct sigaction act;
    
    DEBUGF("load%s", "");

    // needed?
    memset (&act, 0, sizeof (struct sigaction));
    act.sa_handler = SIG_IGN;
    sigfillset (&act.sa_mask);
    sigaction (SIGHUP, &act, NULL);
    

    cb.dtor = (ErlNifResourceDtor*) fuserl_dtor;
    cb.stop = (ErlNifResourceStop*) fuserl_stop;
    cb.down = (ErlNifResourceDown*) fuserl_down;
    
    if ((fuserl_r =
	 enif_open_resource_type_x(env, "fuserl",
				   &cb, ERL_NIF_RT_CREATE,
				   &tried)) == NULL) {
	return -1;
    }
    if (load_atoms(env) < 0)
	return -1;

    hash_helems("reply", reply_hash, REPLY_HASH_SIZE, reply_tab);

    *priv_data = 0;
    return 0;
}

static int upgrade(ErlNifEnv* env, void** priv_data,
		   void** old_priv_data,
		   ERL_NIF_TERM load_info)
{
    UNUSED(env);
    UNUSED(load_info);
    ErlNifResourceFlags tried;    
    ErlNifResourceTypeInit cb;
    
    DEBUGF("upgrade%s", "");

    cb.dtor = (ErlNifResourceDtor*) fuserl_dtor;
    cb.stop = (ErlNifResourceStop*) fuserl_stop;
    cb.down = (ErlNifResourceDown*) fuserl_down;

    if ((fuserl_r = enif_open_resource_type_x(env, "fuserl", &cb,
					    ERL_NIF_RT_CREATE|
					    ERL_NIF_RT_TAKEOVER,
					    &tried)) == NULL) {
	return -1;
    }
    if (load_atoms(env) < 0)
	return -1;    
    *priv_data = *old_priv_data;
    return 0;
}

static void unload(ErlNifEnv* env, void* priv_data)
{
    UNUSED(env);
    UNUSED(priv_data);
    DEBUGF("unload%s", "");
}

ERL_NIF_INIT(fuserl_nif, nif_funcs, load, NULL, upgrade, unload)
