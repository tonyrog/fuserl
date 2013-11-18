#ifndef __FUSERL_TYPES_H_
#define __FUSERL_TYPES_H_ 1

#include <fuse/fuse_lowlevel.h>

enum _FusErlOpLL 
{
  FUSERL_LOOKUP = 0,
  FUSERL_FORGET = 1,
  FUSERL_GETATTR = 2,
  FUSERL_SETATTR = 3,
  FUSERL_READLINK = 4,
  FUSERL_MKNOD = 5,
  FUSERL_MKDIR = 6,
  FUSERL_UNLINK = 7,
  FUSERL_RMDIR = 8,
  FUSERL_SYMLINK = 9,
  FUSERL_RENAME = 10,
  FUSERL_LINK = 11,
  FUSERL_OPEN = 12,
  FUSERL_READ = 13,
  FUSERL_WRITE = 14,
  FUSERL_FLUSH = 15,
  FUSERL_RELEASE = 16,
  FUSERL_FSYNC = 17,
  FUSERL_OPENDIR = 18,
  FUSERL_READDIR = 19,
  FUSERL_RELEASEDIR = 20,
  FUSERL_FSYNCDIR = 21,
  FUSERL_STATFS = 22,
#if HAVE_SETXATTR
  FUSERL_SETXATTR = 23,
  FUSERL_GETXATTR = 24,
  FUSERL_LISTXATTR = 25,
  FUSERL_REMOVEXATTR = 26,
#endif
  FUSERL_ACCESS = 27,
  FUSERL_CREATE = 28,
  FUSERL_GETLK = 29,
  FUSERL_SETLK = 30
};
typedef enum _FusErlOpLL FusErlOpLL;

enum _FuseReplyType 
{
  FUSE_REPLY_ERR = 0,
  FUSE_REPLY_NONE = 1,
  FUSE_REPLY_ENTRY = 2,
  FUSE_REPLY_CREATE = 3,
  FUSE_REPLY_ATTR = 4,
  FUSE_REPLY_READLINK = 5,
  FUSE_REPLY_OPEN = 6,
  FUSE_REPLY_WRITE = 7,
  FUSE_REPLY_BUF = 8,
  /* FUSE_REPLY_IOV = 9,  */
  FUSE_REPLY_STATFS = 10,
#if HAVE_SETXATTR
  FUSE_REPLY_XATTR = 11,
#endif
  FUSE_REPLY_LOCK = 12,
  /* FUSE_REPLY_BMAP = 13, */
  FUSE_REPLY_DIRENTRYLIST = 14  /* this one I made up */
};
typedef enum _FuseReplyType FuseReplyType;

typedef struct _ToEmulatorLL ToEmulatorLL;

struct _FuseReply 
{
  FuseReplyType                                         type;

  fuse_req_t                                            req;

  union
    {
      struct
        {
          int   err;
        }                                       err;
      struct
        {
        }                                       none;
      struct
        {
          struct fuse_entry_param       e;
        }                                       entry;
      struct
        {
          struct fuse_entry_param       e;
          struct fuse_file_info         fi;
        }                                       create;
      struct
        {
          struct stat                   attr;
          double                        attr_timeout;
        }                                       attr;
      struct
        {
          char*                         link;
        }                                       readlink;
      struct
        {
          struct fuse_file_info         fi;
        }                                       open;
      struct
        {
          size_t                        count;
        }                                       write;
      struct
        {
          char*                         buf;
          size_t                        size;
        }                                       buf;
      struct
        {
          struct statvfs                stbuf;
        }                                       statfs;
#if HAVE_SETXATTR
      struct
        {
          size_t                        count;
        }                                       xattr;
#endif
      struct
        {
          struct flock                  lock;
        }                                       lock;
      struct
        {
          char                         *buf;
          size_t                        size;
          size_t                        max;
        }                                       direntrylist;
    }                                                   data;
};
typedef struct _FuseReply FuseReply;

typedef struct _FromEmulatorLL FromEmulatorLL;
struct _FromEmulatorLL 
{
  enum
    {
      EMULATOR_REQUEST_START = 0,
      EMULATOR_REPLY_REPLY = 1,
      EMULATOR_REPLY_INVALID = 255
    }                                           type;

  union
    {
      struct
        {
          char*                 mountopts;
          char*                 mountpoint;
          unsigned int          num_ops;
          char*                 ops;
        }                               start;

      FuseReply                         reply;
    }                                           data;
};

typedef struct _FusErlRequestLL FusErlRequestLL;
struct _FusErlRequestLL 
{
  FusErlOpLL                                                    op;

  fuse_req_t                                                    req;

  uid_t                                                         uid;
  gid_t                                                         gid;
  pid_t                                                         pid;

  union
    {
      struct
        {
          fuse_ino_t                    ino;
          int                           mask;
        }                                       access;

      struct
        {
          fuse_ino_t                    parent;
          const char*                   name;  
          mode_t                        mode;
          struct fuse_file_info*        fi;
        }                                       create;

      struct
        {
          fuse_ino_t                    ino;
          struct fuse_file_info*        fi;
        }                                       flush;

      struct
        {
          fuse_ino_t                    ino;
          unsigned long                 nlookup;
        }                                       forget;

      struct
        {
          fuse_ino_t                    ino;
          int                           datasync;
          struct fuse_file_info*        fi;
        }                                       fsync;

      struct
        {
          fuse_ino_t                    ino;
          int                           datasync;
          struct fuse_file_info*        fi;
        }                                       fsyncdir;

      struct
        {
          fuse_ino_t                    ino;
          struct fuse_file_info*        fi;
        }                                       getattr;

      struct
        {
          fuse_ino_t                    ino;
          struct fuse_file_info*        fi;
          struct flock*                 lock;
        }                                       getlk;

      struct
        {
          fuse_ino_t                    ino;
          const char*                   name;
          size_t                        size;
#if (__FreeBSD__ >= 10) || defined(__APPLE__)
          uint32_t                      position;       // NB: unused (!)
#endif
        }                                       getxattr;

      struct
        {
          fuse_ino_t                    ino;
          fuse_ino_t                    newparent;
          const char*                   newname;  
        }                                       link;

      struct
        {
          fuse_ino_t                    ino;
          size_t                        size;
        }                                       listxattr;

      struct
        {
          fuse_ino_t                    parent;
          const char*                   name;  
        }                                       lookup;

      struct
        {
          fuse_ino_t                    parent;
          const char*                   name;  
          mode_t                        mode;
        }                                       mkdir;

      struct
        {
          fuse_ino_t                    parent;
          const char*                   name;  
          mode_t                        mode;
          dev_t                         rdev;
        }                                       mknod;

      struct
        {
          fuse_ino_t                    ino;
          struct fuse_file_info*        fi;
        }                                       open;

      struct
        {
          fuse_ino_t                    ino;
          struct fuse_file_info*        fi;
        }                                       opendir;

      struct 
        {
          fuse_ino_t                    ino;
          size_t                        size;
          off_t                         off;
          struct fuse_file_info*        fi;
        }                                       read;

      struct
        {
          fuse_ino_t                    ino;
          size_t                        size;
          off_t                         off;
          struct fuse_file_info*        fi;
        }                                       readdir;

      struct
        {
          fuse_ino_t                    ino;
          const char*                   name;
        }                                       removexattr;

      struct
        {
          fuse_ino_t                    ino;
          struct stat*                  attr;
          int                           to_set;
          struct fuse_file_info*        fi;
        }                                       setattr;

      struct
        {
          fuse_ino_t                    ino;
          struct fuse_file_info*        fi;
          struct flock*                 lock;
          int                           sleep;
        }                                       setlk;

      struct
        {
          fuse_ino_t                    ino;
          const char*                   name;
          const char*                   value;
          size_t                        size;
          int                           flags;
#if (__FreeBSD__ >= 10) || defined(__APPLE__)
          uint32_t                      position;       // NB: unused (!)
#endif
        }                                       setxattr;

      struct
        {
          fuse_ino_t                    ino;
        }                                       statfs;

      struct
        {
          fuse_ino_t                    ino;
        }                                       readlink;

      struct
        {
          fuse_ino_t                    ino;
          struct fuse_file_info*        fi;
        }                                       release;

      struct
        {
          fuse_ino_t                    ino;
          struct fuse_file_info*        fi;
        }                                       releasedir;

      struct
        {
          fuse_ino_t                    parent;
          const char*                   name;  
          fuse_ino_t                    newparent;
          const char*                   newname;  
        }                                       rename;

      struct
        {
          fuse_ino_t                    parent;
          const char*                   name;  
        }                                       rmdir;

      struct
        {
          const char*                   link;
          fuse_ino_t                    parent;
          const char*                   name;
        }                                       symlink;

      struct
        {
          fuse_ino_t                    ino;
          const char*                   buf;
          size_t                        size;
          off_t                         off;
          struct fuse_file_info*        fi;
        }                                       write;

      struct
        {
          fuse_ino_t                    parent;
          const char*                   name;  
        }                                       unlink;
    }                                                           data;
};

struct _ToEmulatorLL 
{
  enum
    {
      EMULATOR_REQUEST = 0,
      EMULATOR_REPLY_START = 1
    }                                                   type;

  union
    {
      FusErlRequestLL                   request;

      struct
        {
          enum
            {
              START_OK,
              START_ERROR
            }                   status;
        }                               start;
    }                                                   data;
};

typedef struct _DriverDataLL DriverDataLL;
struct _DriverDataLL 
{
  ErlDrvPort                    port;
  unsigned int                  initialized;
  char*                         mountpoint;
  struct fuse_chan*             chan;
  unsigned int                  selected;

  ToEmulatorLL                  start_reply;

  struct fuse_session*          se;
  struct fuse_lowlevel_ops      op;

  int                           use_pipe;
  int                           filedes[2];
  pthread_t                     read_thread;
  int                           read_thread_init;
  char*                         buf;
};

#endif /* __FUSERL_TYPES_H_ */
