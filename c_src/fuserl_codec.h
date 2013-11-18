#ifndef __FUSERL_CODEC_H_
#define __FUSERL_CODEC_H_

#include <stdint.h>
#include <string.h>
#include <sys/types.h>

#include "fuserl_portability.h"

/*
 * Basic types
 */

#define DECODE_BINARY(xlen, x)                  \
  do                                            \
    {                                           \
      DECODE_NATIVE_64_UNSIGNED (xlen);         \
      (x) = buf;                                \
      buf += xlen;                              \
      buflen -= xlen;                           \
    }                                           \
  while (0)

#define DECODE_BYTE(x)                  \
  do                                    \
    {                                   \
      if (buflen < 1) { goto ERROR; }   \
      (x) = *buf;                       \
      buf += 1;                         \
      buflen -= 1;                      \
    }                                   \
  while (0)

static int64_t
decode_native_64_signed  (char*          buf)
{
  int64_t rv;

  memcpy (&rv, buf, 8);

  return rv;
}

#define DECODE_NATIVE_64_SIGNED(x)              \
  do                                            \
    {                                           \
      if (buflen < 8) { goto ERROR; }           \
      (x) = decode_native_64_signed (buf);      \
      buf += 8;                                 \
      buflen -= 8;                              \
    }                                           \
  while (0)

static uint64_t
decode_native_64_unsigned  (char*          buf)
{
  uint64_t rv;

  memcpy (&rv, buf, 8);

  return rv;
}

#define DECODE_NATIVE_64_UNSIGNED(x)            \
  do                                            \
    {                                           \
      if (buflen < 8) { goto ERROR; }           \
      (x) = decode_native_64_unsigned (buf);    \
      buf += 8;                                 \
      buflen -= 8;                              \
    }                                           \
  while (0)

/*
 * Everywhere this macro is used, it means a bug in the
 * Erlang marshalling code can crash the emulator.  Yikes!
 */

#define DECODE_POINTER(x)                                               \
  do                                                                    \
    {                                                                   \
      if (buflen < 8) { goto ERROR; }                                   \
      (x) = (void*) (uintptr_t) decode_native_64_unsigned (buf);        \
      buf += 8;                                                         \
      buflen -= 8;                                                      \
    }                                                                   \
  while (0)

/* millisecond resolution */
#define DECODE_TIMEOUT(x)                                       \
  do                                                            \
    {                                                           \
      DECODE_NATIVE_64_UNSIGNED (x);                            \
      (x) /= 1000.0;                                            \
    }                                                           \
  while (0)

/*
 * Structures
 */

#define DECODE_DEVICE(x)                        \
  do                                            \
    {                                           \
      uint64_t _maj, _min;                      \
      DECODE_NATIVE_64_UNSIGNED (_maj);         \
      DECODE_NATIVE_64_UNSIGNED (_min);         \
      (x) = makedev (_maj, _min);               \
    }                                           \
  while (0)

#define DECODE_FFI(x)                                                   \
  do                                                                    \
    {                                                                   \
      memset (&(x), 0, sizeof (struct fuse_file_info));                 \
      DECODE_NATIVE_64_SIGNED ((x).flags);                              \
      (x).flags = fuserl_open_flags_decanonicalize ((x).flags);         \
      /* DECODE_NATIVE_64_UNSIGNED ((x).fh_old);                        \ */ \
      DECODE_BYTE ((x).writepage);                                      \
      DECODE_BYTE ((x).direct_io);                                      \
      DECODE_BYTE ((x).keep_cache);                                     \
      DECODE_BYTE ((x).flush);                                          \
      /* DECODE_NATIVE_64_UNSIGNED ((x).padding);                       \ */ \
      DECODE_NATIVE_64_UNSIGNED ((x).fh);                               \
      DECODE_NATIVE_64_UNSIGNED ((x).lock_owner);                       \
    }                                                                   \
  while (0)

#define DECODE_FLOCK(x)                                         \
  do                                                            \
    {                                                           \
      memset (&(x), 0, sizeof (struct flock));                  \
      DECODE_BYTE ((x).l_type);                                 \
      (x).l_type = fuserl_l_type_decanonicalize ((x).l_type);   \
      DECODE_BYTE ((x).l_whence);                               \
      (x).l_whence = fuserl_l_whence_decanonicalize ((x).l_whence);    \
      DECODE_NATIVE_64_UNSIGNED ((x).l_start);                  \
      DECODE_NATIVE_64_UNSIGNED ((x).l_len);                    \
      DECODE_NATIVE_64_UNSIGNED ((x).l_pid);                    \
    }                                                           \
  while (0)

#define DECODE_STAT(x)                                          \
  do                                                            \
    {                                                           \
      memset (&(x), 0, sizeof (struct stat));                   \
      DECODE_DEVICE ((x).st_dev);                               \
      DECODE_NATIVE_64_UNSIGNED ((x).st_ino);                   \
      DECODE_NATIVE_64_UNSIGNED ((x).st_mode);                  \
      (x).st_mode = fuserl_stat_mode_decanonicalize ((x).st_mode);     \
      DECODE_NATIVE_64_UNSIGNED ((x).st_nlink);                 \
      DECODE_NATIVE_64_SIGNED ((x).st_uid);                     \
      DECODE_NATIVE_64_SIGNED ((x).st_gid);                     \
      DECODE_DEVICE ((x).st_rdev);                              \
      DECODE_NATIVE_64_UNSIGNED ((x).st_size);                  \
      DECODE_NATIVE_64_UNSIGNED ((x).st_blksize);               \
      DECODE_NATIVE_64_UNSIGNED ((x).st_blocks);                \
      DECODE_NATIVE_64_UNSIGNED ((x).st_atime);                 \
      DECODE_NATIVE_64_UNSIGNED ((x).st_mtime);                 \
      DECODE_NATIVE_64_UNSIGNED ((x).st_ctime);                 \
    }                                                           \
  while (0)

#define DECODE_STATVFS(x)                               \
  do                                                    \
    {                                                   \
      memset (&(x), 0, sizeof (struct statvfs));        \
      DECODE_NATIVE_64_UNSIGNED ((x).f_bsize);          \
      DECODE_NATIVE_64_UNSIGNED ((x).f_frsize);         \
      DECODE_NATIVE_64_UNSIGNED ((x).f_blocks);         \
      DECODE_NATIVE_64_UNSIGNED ((x).f_bfree);          \
      DECODE_NATIVE_64_UNSIGNED ((x).f_bavail);         \
      DECODE_NATIVE_64_UNSIGNED ((x).f_files);          \
      DECODE_NATIVE_64_UNSIGNED ((x).f_ffree);          \
      DECODE_NATIVE_64_UNSIGNED ((x).f_favail);         \
      DECODE_NATIVE_64_UNSIGNED ((x).f_fsid);           \
      DECODE_NATIVE_64_UNSIGNED ((x).f_flag);           \
      DECODE_NATIVE_64_UNSIGNED ((x).f_namemax);        \
    }                                                   \
  while (0)

#define DECODE_STRING(x)                        \
  do                                            \
    {                                           \
      uint64_t len;                             \
      DECODE_BINARY (len, x);                   \
    }                                           \
  while (0)

#define DECODE_FUSE_ENTRY_PARAM(x)                              \
  do                                                            \
    {                                                           \
      unsigned char have_stbuf;                                 \
      memset (&(x), 0, sizeof (struct fuse_entry_param));       \
      DECODE_NATIVE_64_UNSIGNED ((x).ino);                      \
      DECODE_NATIVE_64_UNSIGNED ((x).generation);               \
      DECODE_BYTE (have_stbuf);                                 \
      if (! have_stbuf) { goto ERROR; }                         \
      DECODE_STAT ((x).attr);                                   \
      DECODE_TIMEOUT ((x).attr_timeout);                        \
      DECODE_TIMEOUT ((x).entry_timeout);                       \
    }                                                           \
  while (0)

#define ENCODE_BYTE(x)          \
  do                            \
    {                           \
      if (buflen < 1)           \
        {                       \
          goto ERROR;           \
        }                       \
                                \
      *buf = (x);               \
      ++buf;                    \
      ++used;                   \
      --buflen;                 \
    }                           \
  while (0)

static void
encode_native_64_signed (char*          buf,
                         int64_t        x)
{
  memcpy (buf, &x, 8);
}

#define ENCODE_NATIVE_64_SIGNED(x)              \
  do                                            \
    {                                           \
      if (buflen < 8)                           \
        {                                       \
          goto ERROR;                           \
        }                                       \
                                                \
      encode_native_64_signed (buf, (x));       \
      buf += 8;                                 \
      used += 8;                                \
      buflen -= 8;                              \
    }                                           \
  while (0)

static void
encode_native_64_unsigned (char*        buf,
                           uint64_t     x)
{
  memcpy (buf, &x, 8);
}

#define ENCODE_NATIVE_64_UNSIGNED(x)            \
  do                                            \
    {                                           \
      if (buflen < 8)                           \
        {                                       \
          goto ERROR;                           \
        }                                       \
                                                \
      encode_native_64_unsigned (buf, (x));     \
      buf += 8;                                 \
      used += 8;                                \
      buflen -= 8;                              \
    }                                           \
  while (0)

#define ENCODE_POINTER(x)                               \
  do                                                    \
    {                                                   \
      ENCODE_NATIVE_64_UNSIGNED ((uintptr_t) (x));      \
    }                                                   \
  while (0)

#define ENCODE_BINARY(xlen, x)                  \
  do                                            \
    {                                           \
      size_t _len = (xlen);                     \
                                                \
      ENCODE_NATIVE_64_UNSIGNED (_len);         \
                                                \
      if (buflen < _len)                        \
        {                                       \
          goto ERROR;                           \
        }                                       \
                                                \
      memcpy (buf, (x), _len);                  \
      buf += _len;                              \
      used += _len;                             \
      buflen -= _len;                           \
    }                                           \
  while (0)

#define ENCODE_STRING(x)                        \
  ENCODE_BINARY (strlen (x), x)

#define ENCODE_DEVICE(x)                        \
  do                                            \
    {                                           \
      ENCODE_NATIVE_64_UNSIGNED (major (x));    \
      ENCODE_NATIVE_64_UNSIGNED (minor (x));    \
    }                                           \
  while (0)

#define ENCODE_FFI(x)                                                   \
  do                                                                    \
    {                                                                   \
      ENCODE_NATIVE_64_SIGNED (fuserl_open_flags_canonicalize ((x).flags));   \
      /* ENCODE_NATIVE_64_UNSIGNED ((x).fh_old);                        \ */ \
      ENCODE_BYTE ((x).writepage);                                      \
      ENCODE_BYTE ((x).direct_io);                                      \
      ENCODE_BYTE ((x).keep_cache);                                     \
      ENCODE_BYTE ((x).flush);                                          \
      /* ENCODE_NATIVE_64_UNSIGNED ((x).padding);                       \ */ \
      ENCODE_NATIVE_64_UNSIGNED ((x).fh);                               \
      ENCODE_NATIVE_64_UNSIGNED ((x).lock_owner);                       \
    }                                                                   \
  while (0)

#define ENCODE_FFI_PTR(x)                                               \
  do                                                                    \
    {                                                                   \
      ENCODE_BYTE (((x) != NULL) ? 0 : 1);                              \
                                                                        \
      if ((x) != NULL)                                                  \
        {                                                               \
          ENCODE_FFI (*(x));                                            \
        }                                                               \
    }                                                                   \
  while (0)

#define ENCODE_FLOCK(x)                                         \
  do                                                            \
    {                                                           \
      ENCODE_BYTE (fuserl_l_type_canonicalize ((x).l_type));    \
      ENCODE_BYTE (fuserl_l_whence_canonicalize ((x).l_whence));\
      ENCODE_NATIVE_64_UNSIGNED ((x).l_start);                  \
      ENCODE_NATIVE_64_UNSIGNED ((x).l_len);                    \
      ENCODE_NATIVE_64_UNSIGNED ((x).l_pid);                    \
    }                                                           \
  while (0)

#define ENCODE_GID(x)                                   \
  do                                                    \
    {                                                   \
      if ((x) == (gid_t) -1)                            \
        {                                               \
          ENCODE_NATIVE_64_SIGNED (-1);                 \
        }                                               \
      else                                              \
        {                                               \
          ENCODE_NATIVE_64_SIGNED (x);                  \
        }                                               \
    }                                                   \
  while (0)

#define ENCODE_PID(x)                                   \
  do                                                    \
    {                                                   \
      if ((x) == (pid_t) -1)                            \
        {                                               \
          ENCODE_NATIVE_64_SIGNED (-1);                 \
        }                                               \
      else                                              \
        {                                               \
          ENCODE_NATIVE_64_SIGNED (x);                  \
        }                                               \
    }                                                   \
  while (0)

#define ENCODE_UID(x)                                   \
  do                                                    \
    {                                                   \
      if ((x) == (uid_t) -1)                            \
        {                                               \
          ENCODE_NATIVE_64_SIGNED (-1);                 \
        }                                               \
      else                                              \
        {                                               \
          ENCODE_NATIVE_64_SIGNED (x);                  \
        }                                               \
    }                                                   \
  while (0)

#define ENCODE_STAT(x)                                          \
  do                                                            \
    {                                                           \
      ENCODE_DEVICE ((x).st_dev);                               \
      ENCODE_NATIVE_64_UNSIGNED ((x).st_ino);                   \
      ENCODE_NATIVE_64_UNSIGNED (                               \
        fuserl_stat_mode_canonicalize ((x).st_mode));           \
      ENCODE_NATIVE_64_UNSIGNED ((x).st_nlink);                 \
      ENCODE_UID ((x).st_uid);                                  \
      ENCODE_GID ((x).st_gid);                                  \
      ENCODE_DEVICE ((x).st_rdev);                              \
      ENCODE_NATIVE_64_UNSIGNED ((x).st_size);                  \
      ENCODE_NATIVE_64_UNSIGNED ((x).st_blksize);               \
      ENCODE_NATIVE_64_UNSIGNED ((x).st_blocks);                \
      ENCODE_NATIVE_64_UNSIGNED ((x).st_atime);                 \
      ENCODE_NATIVE_64_UNSIGNED ((x).st_mtime);                 \
      ENCODE_NATIVE_64_UNSIGNED ((x).st_ctime);                 \
    }                                                           \
  while (0)


#endif /* __FUSERL_CODEC_H_ */
