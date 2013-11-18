#include <erl_driver.h>
#include <errno.h>
#include <fcntl.h>
#include <fuse.h>
#include <inttypes.h>
#include <limits.h>
#include <pthread.h>
#include <stdint.h>
#include <signal.h>
#include <string.h>
#include <sys/mount.h>
#include <sys/param.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/types.h>
#if HAVE_SETXATTR
#include <sys/xattr.h>
#endif
#include <unistd.h>

#include "fuserl_codec.h"
#include "fuserl.h"
#include "fuserltypes.h"

#undef FUSERL_DEBUG

#ifdef FUSERL_DEBUG
#include <stdio.h> 
#define fuserl_debug(format, ...) fprintf (stderr, format, ##__VA_ARGS__)
#else
#define fuserl_debug(format, ...) do { } while (0)
#endif

/*=====================================================================*
 *                         Encoding / decoding                         *
 *=====================================================================*/

static int
is_reply_type   (char reply_type)
{
  switch ((FuseReplyType) reply_type)
    {
      case FUSE_REPLY_ERR:
      case FUSE_REPLY_NONE:
      case FUSE_REPLY_ENTRY:
      case FUSE_REPLY_CREATE:
      case FUSE_REPLY_ATTR:
      case FUSE_REPLY_READLINK:
      case FUSE_REPLY_OPEN:
      case FUSE_REPLY_WRITE:
      case FUSE_REPLY_BUF:
      case FUSE_REPLY_STATFS:
#if HAVE_SETXATTR
      case FUSE_REPLY_XATTR:
#endif
      case FUSE_REPLY_LOCK:
      case FUSE_REPLY_DIRENTRYLIST:
        return 1;
    }

  return 0;
}

static int
is_opcode (char op)
{
  switch ((FusErlOpLL) op)
    {
      case FUSERL_ACCESS:
      case FUSERL_CREATE:
      case FUSERL_FLUSH:
      case FUSERL_FORGET:
      case FUSERL_FSYNC:
      case FUSERL_FSYNCDIR:
      case FUSERL_GETATTR:
      case FUSERL_GETLK:
      case FUSERL_LINK:
      case FUSERL_LOOKUP:
      case FUSERL_MKDIR:
      case FUSERL_MKNOD:
      case FUSERL_OPEN:
      case FUSERL_OPENDIR:
      case FUSERL_READ:
      case FUSERL_READDIR:
      case FUSERL_READLINK:
      case FUSERL_RELEASE:
      case FUSERL_RELEASEDIR:
      case FUSERL_RENAME:
      case FUSERL_RMDIR:
      case FUSERL_SETATTR:
      case FUSERL_SETLK:
      case FUSERL_STATFS:
      case FUSERL_SYMLINK:
      case FUSERL_WRITE:
      case FUSERL_UNLINK:
#if HAVE_SETXATTR
      case FUSERL_GETXATTR:
      case FUSERL_LISTXATTR:
      case FUSERL_REMOVEXATTR:
      case FUSERL_SETXATTR:
#endif
        return 1;
    }

  return 0;
}

#define DIRBUF_ADD(req, buf, size, max, name, stbuf, offset)            \
  do                                                                    \
    {                                                                   \
      size_t incr;                                                      \
                                                                        \
      incr = fuse_add_direntry (req, NULL, 0, name, NULL, 0);           \
                                                                        \
      if (size + incr > max)                                            \
        {                                                               \
          char* nb = driver_realloc (buf, max + MAX (max, incr));       \
                                                                        \
          if (nb == NULL)                                               \
            {                                                           \
              goto ERROR;                                               \
            }                                                           \
                                                                        \
          buf = nb;                                                     \
          max += MAX (max, incr);                                       \
        }                                                               \
                                                                        \
      fuse_add_direntry (req,                                           \
                         buf + size,                                    \
                         max - size,                                    \
                         name,                                          \
                         &stbuf,                                        \
                         offset);                                       \
                                                                        \
      size += incr;                                                     \
    }                                                                   \
  while (0)

static FromEmulatorLL
decode_from     (char*          buf,
                 int            buflen)
{
  FromEmulatorLL from;
  unsigned char type;

  memset (&from, 0, sizeof (FromEmulatorLL));

  DECODE_BYTE (type);

  switch (type)
    {
      case EMULATOR_REQUEST_START:
        DECODE_STRING (from.data.start.mountopts);
        DECODE_STRING (from.data.start.mountpoint);
        DECODE_BYTE (from.data.start.num_ops);
        from.data.start.ops = buf;
        from.type = EMULATOR_REQUEST_START;

        break;

      case EMULATOR_REPLY_REPLY:
        {
          FuseReply* reply;
          char reply_type;

          DECODE_POINTER (from.data.reply.req);
          DECODE_BYTE (reply_type);

          if (! is_reply_type (reply_type))
            {
              goto ERROR;
            }

          reply = &from.data.reply;

          reply->type = reply_type;

          switch (reply->type)
            {
              case FUSE_REPLY_ERR:
                DECODE_NATIVE_64_SIGNED (reply->data.err.err);
                reply->data.err.err = 
                  fuserl_errno_decanonicalize (reply->data.err.err);

                break;

              case FUSE_REPLY_NONE:

                break;

              case FUSE_REPLY_ENTRY:
                DECODE_FUSE_ENTRY_PARAM (reply->data.entry.e);

                break;

              case FUSE_REPLY_CREATE:
                DECODE_FUSE_ENTRY_PARAM (reply->data.create.e);
                DECODE_FFI (reply->data.create.fi);

                break;

              case FUSE_REPLY_ATTR:
                { 
                  unsigned char have_stbuf;

                  DECODE_BYTE (have_stbuf);
                  if (! have_stbuf)
                    {
                      goto ERROR;
                    }

                  DECODE_STAT (reply->data.attr.attr);
                  DECODE_TIMEOUT (reply->data.attr.attr_timeout);
                }

                break;

              case FUSE_REPLY_READLINK:
                DECODE_STRING (reply->data.readlink.link);

                break;

              case FUSE_REPLY_OPEN:
                DECODE_FFI (reply->data.open.fi);

                break;

              case FUSE_REPLY_WRITE:
                DECODE_NATIVE_64_UNSIGNED (reply->data.write.count);

                break;

              case FUSE_REPLY_BUF:
                DECODE_BINARY (reply->data.buf.size,
                               reply->data.buf.buf);

                break;

              case FUSE_REPLY_STATFS:
                DECODE_STATVFS (reply->data.statfs.stbuf);

                break;

#if HAVE_SETXATTR
              case FUSE_REPLY_XATTR:
                DECODE_NATIVE_64_UNSIGNED (reply->data.xattr.count);

                break;
#endif

              case FUSE_REPLY_LOCK:
                DECODE_FLOCK (reply->data.lock.lock);

                break;

              case FUSE_REPLY_DIRENTRYLIST:
                reply->data.direntrylist.buf = driver_alloc (8192);
                reply->data.direntrylist.size = 0;
                reply->data.direntrylist.max = 8192;

                if (reply->data.direntrylist.buf == NULL)
                  {
                    goto ERROR;
                  }

                while (buflen > 0)
                  {
                    char *name;
                    off_t offset;
                    struct stat stbuf;
                    unsigned char have_stbuf;

                    DECODE_STRING (name);
                    DECODE_NATIVE_64_UNSIGNED (offset);

                    DECODE_BYTE (have_stbuf);

                    if (! have_stbuf)
                      {
                        goto ERROR;
                      }

                    DECODE_STAT (stbuf);

                    DIRBUF_ADD (from.data.reply.req,
                                reply->data.direntrylist.buf, 
                                reply->data.direntrylist.size,
                                reply->data.direntrylist.max,
                                name,
                                stbuf,
                                offset);
                  }
            }

          from.type = EMULATOR_REPLY_REPLY;
        }

        break;

      default:
        goto ERROR;
    }

  return from;

ERROR:
  from.type = EMULATOR_REPLY_INVALID;
  return from;
}

static int
encode_msg      (ToEmulatorLL*  msg,
                 char*          buf,
                 size_t         buflen)
{
  size_t used = 0;

  ENCODE_BYTE (msg->type);

  switch (msg->type)
    {
      case EMULATOR_REQUEST:
        ENCODE_BYTE (msg->data.request.op);
        ENCODE_POINTER (msg->data.request.req);
        ENCODE_UID (msg->data.request.uid);
        ENCODE_GID (msg->data.request.gid);
        ENCODE_PID (msg->data.request.pid);
      
        switch (msg->data.request.op)
          {
            case FUSERL_ACCESS:
              ENCODE_NATIVE_64_UNSIGNED (msg->data.request.data.access.ino);
              ENCODE_NATIVE_64_UNSIGNED 
                (fuserl_access_mode_canonicalize 
                  (msg->data.request.data.access.mask));

              break;

            case FUSERL_CREATE:
              ENCODE_NATIVE_64_UNSIGNED (msg->data.request.data.create.parent);
              ENCODE_STRING (msg->data.request.data.create.name);
              ENCODE_NATIVE_64_UNSIGNED 
                (fuserl_stat_mode_canonicalize 
                  (msg->data.request.data.create.mode));
              ENCODE_FFI (*msg->data.request.data.create.fi);

              break;

            case FUSERL_FLUSH:
              ENCODE_NATIVE_64_UNSIGNED (msg->data.request.data.flush.ino);
              ENCODE_FFI (*msg->data.request.data.flush.fi);

              break;

            case FUSERL_FORGET:
              ENCODE_NATIVE_64_UNSIGNED (msg->data.request.data.forget.ino);
              ENCODE_NATIVE_64_UNSIGNED (msg->data.request.data.forget.nlookup);

              break;

            case FUSERL_FSYNC:
              ENCODE_NATIVE_64_UNSIGNED (msg->data.request.data.fsync.ino);
              ENCODE_BYTE (msg->data.request.data.fsync.datasync ? 1 : 0);
              ENCODE_FFI (*msg->data.request.data.fsync.fi);

              break;

            case FUSERL_FSYNCDIR:
              ENCODE_NATIVE_64_UNSIGNED (msg->data.request.data.fsyncdir.ino);
              ENCODE_BYTE (msg->data.request.data.fsyncdir.datasync ? 1 : 0);
              ENCODE_FFI (*msg->data.request.data.fsyncdir.fi);

              break;

            case FUSERL_GETATTR:
              ENCODE_NATIVE_64_UNSIGNED (msg->data.request.data.getattr.ino);

              break;

            case FUSERL_GETLK:
              ENCODE_NATIVE_64_UNSIGNED (msg->data.request.data.getlk.ino);
              ENCODE_FFI (*msg->data.request.data.getlk.fi);
              ENCODE_FLOCK (*msg->data.request.data.getlk.lock);

              break;

            case FUSERL_LINK:
              ENCODE_NATIVE_64_UNSIGNED (msg->data.request.data.link.ino);
              ENCODE_NATIVE_64_UNSIGNED (msg->data.request.data.link.newparent);
              ENCODE_STRING (msg->data.request.data.link.newname);

              break;

            case FUSERL_LOOKUP:
              ENCODE_NATIVE_64_UNSIGNED (msg->data.request.data.lookup.parent);
              ENCODE_STRING (msg->data.request.data.lookup.name);

              break;

            case FUSERL_MKDIR:
              ENCODE_NATIVE_64_UNSIGNED (msg->data.request.data.mkdir.parent);
              ENCODE_STRING (msg->data.request.data.mkdir.name);
              ENCODE_NATIVE_64_UNSIGNED 
                (fuserl_stat_mode_canonicalize 
                   (msg->data.request.data.mkdir.mode));

              break;

            case FUSERL_MKNOD:
              ENCODE_NATIVE_64_UNSIGNED (msg->data.request.data.mknod.parent);
              ENCODE_STRING (msg->data.request.data.mknod.name);
              ENCODE_NATIVE_64_UNSIGNED 
                (fuserl_stat_mode_canonicalize 
                   (msg->data.request.data.mknod.mode));
              ENCODE_DEVICE (msg->data.request.data.mknod.rdev);

              break;

            case FUSERL_OPEN:
              ENCODE_NATIVE_64_UNSIGNED (msg->data.request.data.open.ino);
              ENCODE_FFI (*msg->data.request.data.open.fi);

              break;

            case FUSERL_OPENDIR:
              ENCODE_NATIVE_64_UNSIGNED (msg->data.request.data.opendir.ino);
              ENCODE_FFI (*msg->data.request.data.opendir.fi);

              break;

            case FUSERL_READ:
              ENCODE_NATIVE_64_UNSIGNED (msg->data.request.data.read.ino);
              ENCODE_NATIVE_64_UNSIGNED (msg->data.request.data.read.size);
              ENCODE_NATIVE_64_UNSIGNED (msg->data.request.data.read.off);
              ENCODE_FFI (*msg->data.request.data.read.fi);

              break;

            case FUSERL_READDIR:
              ENCODE_NATIVE_64_UNSIGNED (msg->data.request.data.readdir.ino);
              ENCODE_NATIVE_64_UNSIGNED (msg->data.request.data.readdir.size);
              ENCODE_NATIVE_64_UNSIGNED (msg->data.request.data.readdir.off);
              ENCODE_FFI (*msg->data.request.data.readdir.fi);

              break;

            case FUSERL_READLINK:
              ENCODE_NATIVE_64_UNSIGNED (msg->data.request.data.readlink.ino);

              break;

            case FUSERL_RELEASE:
              ENCODE_NATIVE_64_UNSIGNED (msg->data.request.data.release.ino);
              ENCODE_FFI (*msg->data.request.data.release.fi);

              break;

            case FUSERL_RELEASEDIR:
              ENCODE_NATIVE_64_UNSIGNED (msg->data.request.data.releasedir.ino);
              ENCODE_FFI (*msg->data.request.data.releasedir.fi);

              break;

            case FUSERL_RENAME:
              ENCODE_NATIVE_64_UNSIGNED (msg->data.request.data.rename.parent);
              ENCODE_STRING (msg->data.request.data.rename.name);
              ENCODE_NATIVE_64_UNSIGNED (msg->data.request.data.rename.newparent);
              ENCODE_STRING (msg->data.request.data.rename.newname);

              break;

            case FUSERL_RMDIR:
              ENCODE_NATIVE_64_UNSIGNED (msg->data.request.data.rmdir.parent);
              ENCODE_STRING (msg->data.request.data.rmdir.name);

              break;

            case FUSERL_SETATTR:
              ENCODE_NATIVE_64_UNSIGNED (msg->data.request.data.setattr.ino);
              ENCODE_NATIVE_64_SIGNED (msg->data.request.data.setattr.to_set);
              ENCODE_STAT (*msg->data.request.data.setattr.attr);
              ENCODE_FFI_PTR (msg->data.request.data.setattr.fi);

              break;

            case FUSERL_SETLK:
              ENCODE_NATIVE_64_UNSIGNED (msg->data.request.data.setlk.ino);
              ENCODE_BYTE (msg->data.request.data.setlk.sleep ? 1 : 0);
              ENCODE_FFI (*msg->data.request.data.setlk.fi);
              ENCODE_FLOCK (*msg->data.request.data.setlk.lock);

              break;

            case FUSERL_STATFS:
              ENCODE_NATIVE_64_UNSIGNED (msg->data.request.data.statfs.ino);

              break;

            case FUSERL_SYMLINK:
              ENCODE_STRING (msg->data.request.data.symlink.link);
              ENCODE_NATIVE_64_UNSIGNED (msg->data.request.data.symlink.parent);
              ENCODE_STRING (msg->data.request.data.symlink.name);

              break;

            case FUSERL_WRITE:
              ENCODE_NATIVE_64_UNSIGNED (msg->data.request.data.write.ino);
              ENCODE_BINARY (msg->data.request.data.write.size,
                             msg->data.request.data.write.buf);
              ENCODE_NATIVE_64_UNSIGNED (msg->data.request.data.write.off);
              ENCODE_FFI (*msg->data.request.data.write.fi);

              break;

            case FUSERL_UNLINK:
              ENCODE_NATIVE_64_UNSIGNED (msg->data.request.data.unlink.parent);
              ENCODE_STRING (msg->data.request.data.unlink.name);

              break;

#if HAVE_SETXATTR
            case FUSERL_GETXATTR:
              ENCODE_NATIVE_64_UNSIGNED (msg->data.request.data.getxattr.ino);
              ENCODE_STRING (msg->data.request.data.getxattr.name);
              ENCODE_NATIVE_64_UNSIGNED (msg->data.request.data.getxattr.size);

              break;

            case FUSERL_LISTXATTR:
              ENCODE_NATIVE_64_UNSIGNED (msg->data.request.data.listxattr.ino);
              ENCODE_NATIVE_64_UNSIGNED (msg->data.request.data.listxattr.size);

              break;

            case FUSERL_REMOVEXATTR:
              ENCODE_NATIVE_64_UNSIGNED 
                (msg->data.request.data.removexattr.ino);
              ENCODE_STRING (msg->data.request.data.removexattr.name);

              break;

            case FUSERL_SETXATTR:
              ENCODE_NATIVE_64_UNSIGNED (msg->data.request.data.setxattr.ino);
              ENCODE_STRING (msg->data.request.data.setxattr.name);
              ENCODE_BINARY (msg->data.request.data.setxattr.size,
                             msg->data.request.data.setxattr.value);
              ENCODE_NATIVE_64_SIGNED 
                (fuserl_attr_flags_canonicalize 
                  (msg->data.request.data.setxattr.flags));

              break;
#endif
          }

        break;

      case EMULATOR_REPLY_START:
        switch (msg->data.start.status)
          {
            case START_OK:
              ENCODE_STRING ("ok");

              break;
            case START_ERROR:
              ENCODE_STRING ("error");

              break;
          }

        break;
    }

  return used;

ERROR:
  return -1;
}

static void
do_fuse_reply (FuseReply reply)
{
  switch (reply.type)
    {
      case FUSE_REPLY_ERR:
        fuse_reply_err (reply.req, reply.data.err.err);

        break;

      case FUSE_REPLY_NONE:
        break;

      case FUSE_REPLY_ENTRY:
        fuse_reply_entry (reply.req, &reply.data.entry.e);

        break;

      case FUSE_REPLY_CREATE:
        fuse_reply_create (reply.req,
                           &reply.data.create.e,
                           &reply.data.create.fi);

        break;

      case FUSE_REPLY_ATTR:
        fuse_reply_attr (reply.req,
                         &reply.data.attr.attr,
                         reply.data.attr.attr_timeout);

        break;

      case FUSE_REPLY_READLINK:
        fuse_reply_readlink (reply.req, reply.data.readlink.link);

        break;

      case FUSE_REPLY_OPEN:
        fuse_reply_open (reply.req, &reply.data.open.fi);

        break;

      case FUSE_REPLY_WRITE:
        fuse_reply_write (reply.req, reply.data.write.count);
        
        break;

      case FUSE_REPLY_BUF:
        fuse_reply_buf (reply.req, reply.data.buf.buf, reply.data.buf.size);

        break;

      case FUSE_REPLY_STATFS:
        fuse_reply_statfs (reply.req, &reply.data.statfs.stbuf);

        break;

#if HAVE_SETXATTR
      case FUSE_REPLY_XATTR:
        fuse_reply_xattr (reply.req, reply.data.xattr.count);

        break;
#endif

      case FUSE_REPLY_LOCK:
        fuse_reply_lock (reply.req, &reply.data.lock.lock);

        break;

      case FUSE_REPLY_DIRENTRYLIST:
        fuse_reply_buf (reply.req, 
                        reply.data.direntrylist.buf,
                        reply.data.direntrylist.size);

        break;
    }
}

static int
send_to_emulator (DriverDataLL* d,
                  ToEmulatorLL* msg)
{
  char buf[65536];
  ssize_t size;

  size = encode_msg (msg, buf, sizeof (buf));

  if (size < 0)
    {
      switch (msg->type)
        {
          case EMULATOR_REQUEST:
            { 
              FuseReply reply;

              reply.req = msg->data.request.req;
              reply.type = FUSE_REPLY_ERR;
              reply.data.err.err = -EINVAL;

              do_fuse_reply (reply);
            }

            break;

          case EMULATOR_REPLY_START:
            /* ??? */
            break;
        }
    }
  else
    {
      driver_output (d->port, buf, size);
    }

  return 0;
}

/*=====================================================================*
 *                          fuse_op_callbacks                          *
 *=====================================================================*/

#define FUSE_OPERATION_HEADER                                           \
  DriverDataLL* d = fuse_req_userdata (req);                            \
  ToEmulatorLL msg;                                                     \
  const struct fuse_ctx* ctx = fuse_req_ctx (req);                      \
                                                                        \
  fuserl_debug ("%s start %p\n", __FUNCTION__, req);                    \
                                                                        \
  msg.type = EMULATOR_REQUEST;                                          \
  if (ctx == NULL)                                                      \
    {                                                                   \
      goto ERROR;                                                       \
    }                                                                   \
  msg.data.request.req = req;                                           \
  msg.data.request.uid = ctx->uid;                                      \
  msg.data.request.gid = ctx->gid;                                      \
  msg.data.request.pid = ctx->pid;

#define FUSE_OPERATION_SEND                                             \
  if (send_to_emulator (d, &msg) < 0)                                   \
    {                                                                   \
      goto ERROR;                                                       \
    }

#define FUSE_OPERATION_FOOTER                                           \
  fuserl_debug ("%s complete %p\n", __FUNCTION__, req);                 \
                                                                        \
  return;                                                               \
                                                                        \
ERROR:                                                                  \
  fuse_reply_err (req, -EIO);                                           \
  return;

#define FUSE_OPERATION_IMPL_1(name, nameuc, type1, arg1)                \
static void                                                             \
fuserl_op_ ## name (fuse_req_t req,                                     \
                    type1 arg1)                                         \
{                                                                       \
  FUSE_OPERATION_HEADER                                                 \
                                                                        \
  msg.data.request.op = FUSERL_ ## nameuc,                              \
  msg.data.request.data.name.arg1 = arg1;                               \
                                                                        \
  FUSE_OPERATION_SEND                                                   \
                                                                        \
  FUSE_OPERATION_FOOTER                                                 \
}

#define FUSE_OPERATION_IMPL_2(name, nameuc, type1, arg1, type2, arg2)   \
static void                                                             \
fuserl_op_ ## name (fuse_req_t req,                                     \
                    type1 arg1,                                         \
                    type2 arg2)                                         \
{                                                                       \
  FUSE_OPERATION_HEADER                                                 \
                                                                        \
  msg.data.request.op = FUSERL_ ## nameuc,                              \
  msg.data.request.data.name.arg1 = arg1;                               \
  msg.data.request.data.name.arg2 = arg2;                               \
                                                                        \
  FUSE_OPERATION_SEND                                                   \
                                                                        \
  FUSE_OPERATION_FOOTER                                                 \
}

#define FUSE_OPERATION_IMPL_3(name, nameuc, type1, arg1, type2, arg2, type3, arg3)   \
static void                                                             \
fuserl_op_ ## name (fuse_req_t req,                                     \
                    type1 arg1,                                         \
                    type2 arg2,                                         \
                    type3 arg3)                                         \
{                                                                       \
  FUSE_OPERATION_HEADER                                                 \
                                                                        \
  msg.data.request.op = FUSERL_ ## nameuc,                              \
  msg.data.request.data.name.arg1 = arg1;                               \
  msg.data.request.data.name.arg2 = arg2;                               \
  msg.data.request.data.name.arg3 = arg3;                               \
                                                                        \
  FUSE_OPERATION_SEND                                                   \
                                                                        \
  FUSE_OPERATION_FOOTER                                                 \
}


#define FUSE_OPERATION_IMPL_4(name, nameuc, type1, arg1, type2, arg2, type3, arg3, type4, arg4)   \
static void                                                             \
fuserl_op_ ## name (fuse_req_t req,                                     \
                    type1 arg1,                                         \
                    type2 arg2,                                         \
                    type3 arg3,                                         \
                    type4 arg4)                                         \
{                                                                       \
  FUSE_OPERATION_HEADER                                                 \
                                                                        \
  msg.data.request.op = FUSERL_ ## nameuc,                              \
  msg.data.request.data.name.arg1 = arg1;                               \
  msg.data.request.data.name.arg2 = arg2;                               \
  msg.data.request.data.name.arg3 = arg3;                               \
  msg.data.request.data.name.arg4 = arg4;                               \
                                                                        \
  FUSE_OPERATION_SEND                                                   \
                                                                        \
  FUSE_OPERATION_FOOTER                                                 \
}

#define FUSE_OPERATION_IMPL_5(name, nameuc, type1, arg1, type2, arg2, type3, arg3, type4, arg4, type5, arg5)   \
static void                                                             \
fuserl_op_ ## name (fuse_req_t req,                                     \
                    type1 arg1,                                         \
                    type2 arg2,                                         \
                    type3 arg3,                                         \
                    type4 arg4,                                         \
                    type5 arg5)                                         \
{                                                                       \
  FUSE_OPERATION_HEADER                                                 \
                                                                        \
  msg.data.request.op = FUSERL_ ## nameuc,                              \
  msg.data.request.data.name.arg1 = arg1;                               \
  msg.data.request.data.name.arg2 = arg2;                               \
  msg.data.request.data.name.arg3 = arg3;                               \
  msg.data.request.data.name.arg4 = arg4;                               \
  msg.data.request.data.name.arg5 = arg5;                               \
                                                                        \
  FUSE_OPERATION_SEND                                                   \
                                                                        \
  FUSE_OPERATION_FOOTER                                                 \
}

#define FUSE_OPERATION_IMPL_6(name, nameuc, type1, arg1, type2, arg2, type3, arg3, type4, arg4, type5, arg5, type6, arg6)   \
static void                                                             \
fuserl_op_ ## name (fuse_req_t req,                                     \
                    type1 arg1,                                         \
                    type2 arg2,                                         \
                    type3 arg3,                                         \
                    type4 arg4,                                         \
                    type5 arg5,                                         \
                    type6 arg6)                                         \
{                                                                       \
  FUSE_OPERATION_HEADER                                                 \
                                                                        \
  msg.data.request.op = FUSERL_ ## nameuc,                              \
  msg.data.request.data.name.arg1 = arg1;                               \
  msg.data.request.data.name.arg2 = arg2;                               \
  msg.data.request.data.name.arg3 = arg3;                               \
  msg.data.request.data.name.arg4 = arg4;                               \
  msg.data.request.data.name.arg5 = arg5;                               \
  msg.data.request.data.name.arg6 = arg6;                               \
                                                                        \
  FUSE_OPERATION_SEND                                                   \
                                                                        \
  FUSE_OPERATION_FOOTER                                                 \
}

FUSE_OPERATION_IMPL_2 (access, ACCESS,
                       fuse_ino_t, ino,
                       int, mask)

FUSE_OPERATION_IMPL_4 (create, CREATE,
                       fuse_ino_t, parent,
                       const char*, name,
                       mode_t, mode,
                       struct fuse_file_info*, fi)

FUSE_OPERATION_IMPL_2 (flush, FLUSH,
                       fuse_ino_t, ino,
                       struct fuse_file_info*, fi)

FUSE_OPERATION_IMPL_2 (forget, FORGET, 
                       fuse_ino_t, ino,
                       unsigned long, nlookup)

FUSE_OPERATION_IMPL_3 (fsync, FSYNC,
                       fuse_ino_t, ino,
                       int, datasync,
                       struct fuse_file_info*, fi)

FUSE_OPERATION_IMPL_3 (fsyncdir, FSYNCDIR,
                       fuse_ino_t, ino,
                       int, datasync,
                       struct fuse_file_info*, fi)

FUSE_OPERATION_IMPL_2 (getattr, GETATTR,
                       fuse_ino_t, ino,
                       struct fuse_file_info*, fi)

FUSE_OPERATION_IMPL_3 (getlk, GETLK,
                       fuse_ino_t, ino,
                       struct fuse_file_info*, fi,
                       struct flock*, lock)

FUSE_OPERATION_IMPL_3 (link, LINK,
                       fuse_ino_t, ino,
                       fuse_ino_t, newparent,
                       const char*, newname)

FUSE_OPERATION_IMPL_2 (lookup, LOOKUP, 
                       fuse_ino_t, parent,
                       const char*, name)

FUSE_OPERATION_IMPL_3 (mkdir, MKDIR,
                       fuse_ino_t, parent,
                       const char*, name,
                       mode_t, mode)

FUSE_OPERATION_IMPL_4 (mknod, MKNOD,
                       fuse_ino_t, parent,
                       const char*, name,
                       mode_t, mode,
                       dev_t, rdev)

FUSE_OPERATION_IMPL_2 (open, OPEN,
                       fuse_ino_t, ino,
                       struct fuse_file_info*, fi)

FUSE_OPERATION_IMPL_2 (opendir, OPENDIR,
                       fuse_ino_t, ino,
                       struct fuse_file_info*, fi)

FUSE_OPERATION_IMPL_4 (read, READ,
                       fuse_ino_t, ino,
                       size_t, size,
                       off_t, off,
                       struct fuse_file_info*, fi)

FUSE_OPERATION_IMPL_4 (readdir, READDIR,
                       fuse_ino_t, ino,
                       size_t, size,
                       off_t, off,
                       struct fuse_file_info*, fi)

FUSE_OPERATION_IMPL_1 (readlink, READLINK,
                       fuse_ino_t, ino)

FUSE_OPERATION_IMPL_2 (release, RELEASE,
                       fuse_ino_t, ino,
                       struct fuse_file_info*, fi)

FUSE_OPERATION_IMPL_2 (releasedir, RELEASEDIR,
                       fuse_ino_t, ino,
                       struct fuse_file_info*, fi)

FUSE_OPERATION_IMPL_4 (rename, RENAME,
                       fuse_ino_t, parent,
                       const char*, name,
                       fuse_ino_t, newparent,
                       const char*, newname)

FUSE_OPERATION_IMPL_2 (rmdir, RMDIR,
                       fuse_ino_t, parent,
                       const char*, name)

FUSE_OPERATION_IMPL_4 (setattr, SETATTR,
                       fuse_ino_t, ino,
                       struct stat*, attr,
                       int, to_set,
                       struct fuse_file_info*, fi)

FUSE_OPERATION_IMPL_4 (setlk, SETLK,
                       fuse_ino_t, ino,
                       struct fuse_file_info*, fi,
                       struct flock*, lock,
                       int, sleep)

FUSE_OPERATION_IMPL_1 (statfs, STATFS,
                       fuse_ino_t, ino)

FUSE_OPERATION_IMPL_3 (symlink, SYMLINK,
                       const char*, link,
                       fuse_ino_t, parent,
                       const char*, name)

FUSE_OPERATION_IMPL_5 (write, WRITE,
                       fuse_ino_t, ino,
                       const char*, buf,
                       size_t, size,
                       off_t, off,
                       struct fuse_file_info*, fi)

FUSE_OPERATION_IMPL_2 (unlink, UNLINK,
                       fuse_ino_t, parent,
                       const char*, name)

#if HAVE_SETXATTR
#if (__FreeBSD__ >= 10) || defined(__APPLE__)
FUSE_OPERATION_IMPL_4 (getxattr, GETXATTR,
                       fuse_ino_t, ino,
                       const char*, name,
                       size_t, size,
                       uint32_t, position);     // NB: position is ignored
#else
FUSE_OPERATION_IMPL_3 (getxattr, GETXATTR,
                       fuse_ino_t, ino,
                       const char*, name,
                       size_t, size)
#endif // (__FreeBSD__ >= 10)  || defined(__APPLE__)

FUSE_OPERATION_IMPL_2 (listxattr, LISTXATTR,
                       fuse_ino_t, ino,
                       size_t, size)

FUSE_OPERATION_IMPL_2 (removexattr, REMOVEXATTR,
                       fuse_ino_t, ino,
                       const char*, name)

#if (__FreeBSD__ >= 10) || defined(__APPLE__)
FUSE_OPERATION_IMPL_6 (setxattr, SETXATTR,
                       fuse_ino_t, ino,
                       const char*, name,
                       const char*, value,
                       size_t, size,
                       int, flags,
                       uint32_t, position)      // NB: position is ignored
#else
FUSE_OPERATION_IMPL_5 (setxattr, SETXATTR,
                       fuse_ino_t, ino,
                       const char*, name,
                       const char*, value,
                       size_t, size,
                       int, flags)
#endif // (__FreeBSD__ >= 10) || defined(__APPLE__)
#endif

static void
driver_data_destroy (DriverDataLL* d)
{
  if (d->chan)
    {
      fuse_session_remove_chan (d->chan);
    }

  if (d->se)
    {
      fuse_session_destroy (d->se);
    }

  if (d->use_pipe)
    {
      close (d->filedes[0]);
    }

  if (! d->read_thread_init)
    {
      if (d->buf)
        {
          driver_free (d->buf);
        }

      if (d->use_pipe)
        {
          close (d->filedes[1]);
        }
    }

  if (d->selected)
    {
      driver_select (d->port, 
                     (ErlDrvEvent) (intptr_t) 
                       ((d->use_pipe) ? d->filedes[0] : fuse_chan_fd (d->chan)),
                     DO_READ,
                     0);
    }

  if (d->chan)
    {
#if DARWIN_TWEAKS
      /*
       * MacFuse waits for us to respond to the unmount, but we're
       * way past that.
       */
      fuse_chan_destroy (d->chan);
      unmount (d->mountpoint, MNT_FORCE);
#else
      fuse_unmount (d->mountpoint, d->chan);
#endif
    }

  if (d->mountpoint)
    {
      free (d->mountpoint);
    }

  driver_free ((char*) d);
}

#define __u32 uint32_t
#define __u64 uint64_t

static const size_t sizeof_fuse_in_header = sizeof (
struct fuse_in_header {
        __u32   len;
        __u32   opcode;
        __u64   unique;
        __u64   nodeid;
        __u32   uid;
        __u32   gid;
        __u32   pid;
        __u32   padding;
});

#if DARWIN_TWEAKS

/*
 * Can't poll/kqueue the fuse fd under macfuse, hence funny business.
 */

static size_t
io_exact (int           fd,
          char*         buf,
          size_t        len,
          ssize_t     (*f) (int, void *, size_t))
{
  size_t got = 0;

  while (got < len)
    {
      int r;

      if ((r = f (fd, buf + got, len - got)) <= 0)
        {
          if (r == 0 || errno != EINTR)
            {
              return got;
            }
        }
      else
        {
          got += r;
        }
    }

  return got;
}

static size_t
read_exact (int         fd, 
            char       *buf,
            size_t      len)
{
  return io_exact (fd, buf, len, read);
}

static size_t
write_exact (int        fd, 
             char*      buf,
             size_t     len)
{
  return io_exact (fd, buf, len, (ssize_t (*) (int, void *, size_t)) write);
}

static void*
read_loop (void* arg)
{
  DriverDataLL* d = (DriverDataLL*) arg;
  int read_fd = fuse_chan_fd (d->chan);
  int write_fd = d->filedes[1];
  size_t size = fuse_chan_bufsize (d->chan);
  char *buf = d->buf;

  for (;;)
    {
      int res;

      res = read (read_fd, buf, size);

      if (res < 0)
        {
          if (res != EINTR && res != EAGAIN)
            {
              break;
            }
        }
      else if (res == 0)
        {
          break;
        }
      else if ((size_t) res < sizeof_fuse_in_header)
        {
          break;
        }
      else
        {
          char zero = 0;

          if (write_exact (write_fd, &zero, 1) != 1)
            { 
              break;
            }

          if (read_exact (write_fd, &zero, 1) != 1)
            {
              break;
            }
        }
    }

  driver_free (buf);

  close (write_fd);

  return NULL;
}

#endif /* DARWIN_TWEAKS */

/*=====================================================================*
 *                       Erlang driver callbacks                       *
 *=====================================================================*/

/*
 * actual_start
 * 
 * I needed to pass some arguments to fuse but the Erlang start
 * hook doesn't allow for extra data (?), so starting is deferred until
 * the Erlang code can send the initialization data in a normal command.
 */

#define DEFINE_IMPL(lc, uc)     \
  case FUSERL_ ## uc:           \
    d->op.lc = fuserl_op_ ## lc;\
    break;

static void
actual_start    (DriverDataLL*  d,
                 FromEmulatorLL from)
{
  unsigned int i;
  struct fuse_args margs = FUSE_ARGS_INIT (0, NULL);

  if (d->initialized)
    {
      goto ERROR;
    }

  if (   strlen (from.data.start.mountopts) > 0 
      && (   fuse_opt_add_arg (&margs, "") == -1 
          || fuse_opt_add_arg (&margs, "-o") == -1
          || fuse_opt_add_arg (&margs, from.data.start.mountopts) == -1))
    {
      goto ERROR;
    }

  {
    int j;
    for (j = 0; j < margs.argc; ++j)
      {
        fuserl_debug ("margs[%u] = '%s'\n", j, margs.argv[j]);
      }
  }

  d->mountpoint = strdup (from.data.start.mountpoint);

  if (d->mountpoint == NULL)
    {
      goto ERROR;
    }

  d->chan = fuse_mount (d->mountpoint, &margs);

  if (d->chan == NULL)
    {
      goto ERROR;
    }

  d->buf = driver_alloc (fuse_chan_bufsize (d->chan));

  if (d->buf == NULL)
    {
      goto ERROR;
    }

#if DARWIN_TWEAKS
  if (socketpair (AF_UNIX, SOCK_STREAM, 0, d->filedes) < 0)
    {
      goto ERROR;
    }

  d->use_pipe = 1;

  if (pthread_create (&d->read_thread, NULL, read_loop, d) < 0)
    {
      goto ERROR;
    }

  d->read_thread_init = 1;

  pthread_detach (d->read_thread);
#endif

  if (driver_select (d->port, 
                     (ErlDrvEvent) (intptr_t) 
                       ((d->use_pipe) ? d->filedes[0] : fuse_chan_fd (d->chan)),
                     DO_READ,
                     1) == -1)
    {
      goto ERROR;
    }

  d->selected = 1;

  memset (&d->op, 0, sizeof (d->op));

  for (i = 0; i < from.data.start.num_ops; ++i)
    {
      if (is_opcode (from.data.start.ops[i]))
        {
          switch ((FusErlOpLL) from.data.start.ops[i])
            {
              DEFINE_IMPL (access, ACCESS)
              DEFINE_IMPL (create, CREATE)
              DEFINE_IMPL (flush, FLUSH)
              DEFINE_IMPL (forget, FORGET)
              DEFINE_IMPL (fsync, FSYNC)
              DEFINE_IMPL (fsyncdir, FSYNCDIR)
              DEFINE_IMPL (getattr, GETATTR)
              DEFINE_IMPL (getlk, GETLK)
              DEFINE_IMPL (link, LINK)
              DEFINE_IMPL (lookup, LOOKUP)
              DEFINE_IMPL (mkdir, MKDIR)
              DEFINE_IMPL (mknod, MKNOD)
              DEFINE_IMPL (open, OPEN)
              DEFINE_IMPL (opendir, OPENDIR)
              DEFINE_IMPL (read, READ)
              DEFINE_IMPL (readdir, READDIR)
              DEFINE_IMPL (readlink, READLINK)
              DEFINE_IMPL (release, RELEASE)
              DEFINE_IMPL (releasedir, RELEASEDIR)
              DEFINE_IMPL (rename, RENAME)
              DEFINE_IMPL (rmdir, RMDIR)
              DEFINE_IMPL (setattr, SETATTR)
              DEFINE_IMPL (setlk, SETLK)
              DEFINE_IMPL (statfs, STATFS)
              DEFINE_IMPL (symlink, SYMLINK)
              DEFINE_IMPL (write, WRITE)
              DEFINE_IMPL (unlink, UNLINK)
#if HAVE_SETXATTR
              DEFINE_IMPL (getxattr, GETXATTR)
              DEFINE_IMPL (listxattr, LISTXATTR)
              DEFINE_IMPL (removexattr, REMOVEXATTR)
              DEFINE_IMPL (setxattr, SETXATTR)
#endif
            }
        }
    }

  d->se = fuse_lowlevel_new (&margs, &d->op, sizeof (d->op), d);

  if (d->se == NULL)
    {
      goto ERROR;
    }

  fuse_session_add_chan (d->se, d->chan);

  d->initialized = 1;

  d->start_reply.type = EMULATOR_REPLY_START;
  d->start_reply.data.start.status = START_OK;

  send_to_emulator (d, &d->start_reply);

  fuse_opt_free_args (&margs);
  return;

ERROR:
  d->start_reply.type = EMULATOR_REPLY_START;
  d->start_reply.data.start.status = START_ERROR;

  send_to_emulator (d, &d->start_reply);
  fuse_opt_free_args (&margs);
}

static ErlDrvData
fuserl_start    (ErlDrvPort     port,
                 char*          buf)
{
  DriverDataLL* d = (DriverDataLL*) driver_alloc (sizeof (DriverDataLL));
  (void) buf;

  memset (d, 0, sizeof (DriverDataLL));

  d->port = port;

  return (ErlDrvData) d;
}

static void
fuserl_stop  (ErlDrvData     handle)
{
  DriverDataLL* d = (DriverDataLL*) handle;

  fuserl_debug ("%s %u\n",
                __FUNCTION__,
                d->selected);

  driver_data_destroy (d);

  fuserl_debug ("fuserl_stop complete\n");
}

static void
from_emulator_destruct (FromEmulatorLL from_emulator)
{
  switch (from_emulator.type)
    {
      case EMULATOR_REPLY_INVALID:
        break;

      case EMULATOR_REQUEST_START:
        break;

      case EMULATOR_REPLY_REPLY:
        switch (from_emulator.data.reply.type)
          {
            case FUSE_REPLY_ERR:
            case FUSE_REPLY_NONE:
            case FUSE_REPLY_ENTRY:
            case FUSE_REPLY_CREATE:
            case FUSE_REPLY_ATTR:
            case FUSE_REPLY_READLINK:
            case FUSE_REPLY_OPEN:
            case FUSE_REPLY_WRITE:
            case FUSE_REPLY_BUF:
            case FUSE_REPLY_STATFS:
#if HAVE_SETXATTR
            case FUSE_REPLY_XATTR:
#endif
            case FUSE_REPLY_LOCK:
              break;

            case FUSE_REPLY_DIRENTRYLIST:
              if (from_emulator.data.reply.data.direntrylist.buf != NULL)
                {
                  driver_free (from_emulator.data.reply.data.direntrylist.buf);
                }

              break;
            }

        break;
    }
}

static void
fuserl_output           (ErlDrvData     handle,
                         char*          buf,
                         ErlDrvSizeT    buflen)
{
  DriverDataLL* d = (DriverDataLL*) handle;
  FromEmulatorLL from_emulator = decode_from (buf, buflen);

  fuserl_debug ("fuserl_output\n");

  switch (from_emulator.type)
    {
      case EMULATOR_REPLY_INVALID:
        break;

      case EMULATOR_REQUEST_START:
        actual_start (d, from_emulator);

        break;

      case EMULATOR_REPLY_REPLY:
        /* ASSUME: the fuse_reply_XXX functions call writev on the kernel 
         * file descriptor; i'm assuming for now these won't block
         * so that i don't block the emulator */

        do_fuse_reply (from_emulator.data.reply);

        break;
    }

  from_emulator_destruct (from_emulator);

  fuserl_debug ("fuserl_output complete\n");
}

static void
fuserl_ready_input      (ErlDrvData     handle,
                         ErlDrvEvent    event)
{
  DriverDataLL* d = (DriverDataLL*) handle;
  size_t size = fuse_chan_bufsize (d->chan);
  char *buf = d->buf;
  ssize_t res;

  (void) event;

  fuserl_debug ("fuserl_ready_input\n");

  if (d->use_pipe)
    {
      char byte;

      res = read (d->filedes[0], &byte, 1);
    }
  else
    {
      res = read (fuse_chan_fd (d->chan), buf, size);
    }

  if (res < 0)
    {
      if (res != EINTR && res != EAGAIN)
        {
          driver_failure_posix (d->port, errno);
        }
    }
  else if (res == 0)
    {
      driver_failure_eof (d->port);
    }
  else if (! d->use_pipe && (size_t) res < sizeof_fuse_in_header)
    {
      driver_failure_posix (d->port, EIO);
    }
  else
    {
      fuse_session_process (d->se, buf, res, d->chan);
    }

  if (d->use_pipe)
    {
      char byte;

              /* XXX: Compiler hangs on warnings,
               *       including not dealing with a return value. (Is there a better way to temporarily disable -Werror=unused-result ?)
               */
      if(write (d->filedes[0], &byte, 1)){
      }
    }

  fuserl_debug ("fuserl_ready_input complete\n");
}

static int
fuserl_init (void)
{
  struct sigaction act;

  /*
   * Somewhere in the bowels of fuse, SIGHUP is generated.
   * This will nuke the emulator unless we do something about it.
   */

  memset (&act, 0, sizeof (struct sigaction));
  act.sa_handler = SIG_IGN;
  sigfillset (&act.sa_mask);

  sigaction (SIGHUP, &act, NULL);

  return 0;
}

// FIXME: probably need stop_select ? 
static ErlDrvEntry fuserl_driver_entry = 
{
  .driver_name = (char*) "fuserl",
  .init = fuserl_init,
  .start = fuserl_start, 
  .stop = fuserl_stop,
  .output = fuserl_output,
  .ready_input = fuserl_ready_input,
  .extended_marker = ERL_DRV_EXTENDED_MARKER,
  .major_version = ERL_DRV_EXTENDED_MAJOR_VERSION,
  .minor_version = ERL_DRV_EXTENDED_MINOR_VERSION,
  .driver_flags = ERL_DRV_FLAG_USE_PORT_LOCKING
  
};

DRIVER_INIT (libfuserl) /* must match name in driver_entry */
{
  return &fuserl_driver_entry;
}
