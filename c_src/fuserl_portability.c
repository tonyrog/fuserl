#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>
#if HAVE_SETXATTR
#include <sys/xattr.h>
#endif
#include <unistd.h>

#include "fuserl_portability.h"

/*=====================================================================*
 *                               Private                               *
 *=====================================================================*/

typedef struct _ErrnoMap ErrnoMap;
struct _ErrnoMap 
{
  int canonical;
  int native;
};

static const ErrnoMap errno_map[] = {
#ifdef E2BIG
  { 0, E2BIG },
#endif /* E2BIG */
#ifdef EACCES
  { 1, EACCES },
#endif /* EACCES */
#ifdef EADDRINUSE
  { 2, EADDRINUSE },
#endif /* EADDRINUSE */
#ifdef EADDRNOTAVAIL
  { 3, EADDRNOTAVAIL },
#endif /* EADDRNOTAVAIL */
#ifdef EADV
  { 4, EADV },
#endif /* EADV */
#ifdef EAFNOSUPPORT
  { 5, EAFNOSUPPORT },
#endif /* EAFNOSUPPORT */
#ifdef EAGAIN
  { 6, EAGAIN },
#endif /* EAGAIN */
#ifdef EALIGN
  { 7, EALIGN },
#endif /* EALIGN */
#ifdef EALREADY
  { 8, EALREADY },
#endif /* EALREADY */
#ifdef EBADE
  { 9, EBADE },
#endif /* EBADE */
#ifdef EBADF
  { 10, EBADF },
#endif /* EBADF */
#ifdef EBADFD
  { 11, EBADFD },
#endif /* EBADFD */
#ifdef EBADMSG
  { 12, EBADMSG },
#endif /* EBADMSG */
#ifdef EBADR
  { 13, EBADR },
#endif /* EBADR */
#ifdef EBADRPC
  { 14, EBADRPC },
#endif /* EBADRPC */
#ifdef EBADRQC
  { 15, EBADRQC },
#endif /* EBADRQC */
#ifdef EBADSLT
  { 16, EBADSLT },
#endif /* EBADSLT */
#ifdef EBFONT
  { 17, EBFONT },
#endif /* EBFONT */
#ifdef EBUSY
  { 18, EBUSY },
#endif /* EBUSY */
#ifdef ECHILD
  { 19, ECHILD },
#endif /* ECHILD */
#ifdef ECHRNG
  { 20, ECHRNG },
#endif /* ECHRNG */
#ifdef ECOMM
  { 21, ECOMM },
#endif /* ECOMM */
#ifdef ECONNABORTED
  { 22, ECONNABORTED },
#endif /* ECONNABORTED */
#ifdef ECONNREFUSED
  { 23, ECONNREFUSED },
#endif /* ECONNREFUSED */
#ifdef ECONNRESET
  { 24, ECONNRESET },
#endif /* ECONNRESET */
#ifdef EDEADLK
  { 25, EDEADLK },
#endif /* EDEADLK */
#ifdef EDEADLOCK
  { 26, EDEADLOCK },
#endif /* EDEADLOCK */
#ifdef EDESTADDRREQ
  { 27, EDESTADDRREQ },
#endif /* EDESTADDRREQ */
#ifdef EDIRTY
  { 28, EDIRTY },
#endif /* EDIRTY */
#ifdef EDOM
  { 29, EDOM },
#endif /* EDOM */
#ifdef EDOTDOT
  { 30, EDOTDOT },
#endif /* EDOTDOT */
#ifdef EDQUOT
  { 31, EDQUOT },
#endif /* EDQUOT */
#ifdef EDUPPKG
  { 32, EDUPPKG },
#endif /* EDUPPKG */
#ifdef EEXIST
  { 33, EEXIST },
#endif /* EEXIST */
#ifdef EFAULT
  { 34, EFAULT },
#endif /* EFAULT */
#ifdef EFBIG
  { 35, EFBIG },
#endif /* EFBIG */
#ifdef EHOSTDOWN
  { 36, EHOSTDOWN },
#endif /* EHOSTDOWN */
#ifdef EHOSTUNREACH
  { 37, EHOSTUNREACH },
#endif /* EHOSTUNREACH */
#ifdef EIDRM
  { 38, EIDRM },
#endif /* EIDRM */
#ifdef EINIT
  { 39, EINIT },
#endif /* EINIT */
#ifdef EINPROGRESS
  { 40, EINPROGRESS },
#endif /* EINPROGRESS */
#ifdef EINTR
  { 41, EINTR },
#endif /* EINTR */
#ifdef EINVAL
  { 42, EINVAL },
#endif /* EINVAL */
#ifdef EIO
  { 43, EIO },
#endif /* EIO */
#ifdef EISCONN
  { 44, EISCONN },
#endif /* EISCONN */
#ifdef EISDIR
  { 45, EISDIR },
#endif /* EISDIR */
#ifdef EISNAM
  { 46, EISNAM },
#endif /* EISNAM */
#ifdef ELBIN
  { 47, ELBIN },
#endif /* ELBIN */
#ifdef EL2HLT
  { 48, EL2HLT },
#endif /* EL2HLT */
#ifdef EL2NSYNC
  { 49, EL2NSYNC },
#endif /* EL2NSYNC */
#ifdef EL3HLT
  { 50, EL3HLT },
#endif /* EL3HLT */
#ifdef EL3RST
  { 51, EL3RST },
#endif /* EL3RST */
#ifdef ELIBACC
  { 52, ELIBACC },
#endif /* ELIBACC */
#ifdef ELIBBAD
  { 53, ELIBBAD },
#endif /* ELIBBAD */
#ifdef ELIBEXEC
  { 54, ELIBEXEC },
#endif /* ELIBEXEC */
#ifdef ELIBSCN
  { 57, ELIBSCN },
#endif /* ELIBSCN */
#ifdef ELNRNG
  { 58, ELNRNG },
#endif /* ELNRNG */
#ifdef ELOOP
  { 59, ELOOP },
#endif /* ELOOP */
#ifdef EMFILE
  { 60, EMFILE },
#endif /* EMFILE */
#ifdef EMLINK
  { 61, EMLINK },
#endif /* EMLINK */
#ifdef EMSGSIZE
  { 62, EMSGSIZE },
#endif /* EMSGSIZE */
#ifdef EMULTIHOP
  { 63, EMULTIHOP },
#endif /* EMULTIHOP */
#ifdef ENAMETOOLONG
  { 64, ENAMETOOLONG },
#endif /* ENAMETOOLONG */
#ifdef ENAVAIL
  { 65, ENAVAIL },
#endif /* ENAVAIL */
#ifdef ENET
  { 66, ENET },
#endif /* ENET */
#ifdef ENETDOWN
  { 67, ENETDOWN },
#endif /* ENETDOWN */
#ifdef ENETRESET
  { 68, ENETRESET },
#endif /* ENETRESET */
#ifdef ENETUNREACH
  { 69, ENETUNREACH },
#endif /* ENETUNREACH */
#ifdef ENFILE
  { 70, ENFILE },
#endif /* ENFILE */
#ifdef ENOANO
  { 71, ENOANO },
#endif /* ENOANO */
#ifdef ENOBUFS
  { 72, ENOBUFS },
#endif /* ENOBUFS */
#ifdef ENOCSI
  { 73, ENOCSI },
#endif /* ENOCSI */
#ifdef ENODATA
  { 74, ENODATA },
#endif /* ENODATA */
#ifdef ENODEV
  { 75, ENODEV },
#endif /* ENODEV */
#ifdef ENOENT
  { 76, ENOENT },
#endif /* ENOENT */
#ifdef ENOEXEC
  { 77, ENOEXEC },
#endif /* ENOEXEC */
#ifdef ENOLCK
  { 78, ENOLCK },
#endif /* ENOLCK */
#ifdef ENOLINK
  { 79, ENOLINK },
#endif /* ENOLINK */
#ifdef ENOMEM
  { 80, ENOMEM },
#endif /* ENOMEM */
#ifdef ENOMSG
  { 81, ENOMSG },
#endif /* ENOMSG */
#ifdef ENONET
  { 82, ENONET },
#endif /* ENONET */
#ifdef ENOPKG
  { 83, ENOPKG },
#endif /* ENOPKG */
#ifdef ENOPROTOOPT
  { 84, ENOPROTOOPT },
#endif /* ENOPROTOOPT */
#ifdef ENOSPC
  { 85, ENOSPC },
#endif /* ENOSPC */
#ifdef ENOSR
  { 86, ENOSR },
#endif /* ENOSR */
#ifdef ENOSYM
  { 87, ENOSYM },
#endif /* ENOSYM */
#ifdef ENOSYS
  { 88, ENOSYS },
#endif /* ENOSYS */
#ifdef ENOTBLK
  { 89, ENOTBLK },
#endif /* ENOTBLK */
#ifdef ENOTCONN
  { 90, ENOTCONN },
#endif /* ENOTCONN */
#ifdef ENOTDIR
  { 91, ENOTDIR },
#endif /* ENOTDIR */
#ifdef ENOTEMPTY
  { 92, ENOTEMPTY },
#endif /* ENOTEMPTY */
#ifdef ENOTNAM
  { 93, ENOTNAM },
#endif /* ENOTNAM */
#ifdef ENOTSOCK
  { 94, ENOTSOCK },
#endif /* ENOTSOCK */
#ifdef ENOTSUP
  { 95, ENOTSUP },
#endif /* ENOTSUP */
#ifdef ENOTTY
  { 96, ENOTTY },
#endif /* ENOTTY */
#ifdef ENOTUNIQ
  { 97, ENOTUNIQ },
#endif /* ENOTUNIQ */
#ifdef ENXIO
  { 98, ENXIO },
#endif /* ENXIO */
#ifdef EOPNOTSUPP
  { 99, EOPNOTSUPP },
#endif /* EOPNOTSUPP */
#ifdef EPERM
  { 100, EPERM },
#endif /* EPERM */
#ifdef EPFNOSUPPORT
  { 101, EPFNOSUPPORT },
#endif /* EPFNOSUPPORT */
#ifdef EPIPE
  { 102, EPIPE },
#endif /* EPIPE */
#ifdef EPROCLIM
  { 103, EPROCLIM },
#endif /* EPROCLIM */
#ifdef EPROCUNAVAIL
  { 104, EPROCUNAVAIL },
#endif /* EPROCUNAVAIL */
#ifdef EPROGMISMATCH
  { 105, EPROGMISMATCH },
#endif /* EPROGMISMATCH */
#ifdef EPROGUNAVAIL
  { 106, EPROGUNAVAIL },
#endif /* EPROGUNAVAIL */
#ifdef EPROTO
  { 107, EPROTO },
#endif /* EPROTO */
#ifdef EPROTONOSUPPORT
  { 108, EPROTONOSUPPORT },
#endif /* EPROTONOSUPPORT */
#ifdef EPROTOTYPE
  { 109, EPROTOTYPE },
#endif /* EPROTOTYPE */
#ifdef ERANGE
  { 110, ERANGE },
#endif /* ERANGE */
#ifdef EREFUSED
  { 111, EREFUSED },
#endif /* EREFUSED */
#ifdef EREMCHG
  { 112, EREMCHG },
#endif /* EREMCHG */
#ifdef EREMDEV
  { 113, EREMDEV },
#endif /* EREMDEV */
#ifdef EREMOTE
  { 114, EREMOTE },
#endif /* EREMOTE */
#ifdef EREMOTEIO
  { 115, EREMOTEIO },
#endif /* EREMOTEIO */
#ifdef EREMOTERELEASE
  { 116, EREMOTERELEASE },
#endif /* EREMOTERELEASE */
#ifdef EROFS
  { 117, EROFS },
#endif /* EROFS */
#ifdef ERPCMISMATCH
  { 118, ERPCMISMATCH },
#endif /* ERPCMISMATCH */
#ifdef ERREMOTE
  { 119, ERREMOTE },
#endif /* ERREMOTE */
#ifdef ESHUTDOWN
  { 120, ESHUTDOWN },
#endif /* ESHUTDOWN */
#ifdef ESOCKTNOSUPPORT
  { 121, ESOCKTNOSUPPORT },
#endif /* ESOCKTNOSUPPORT */
#ifdef ESPIPE
  { 122, ESPIPE },
#endif /* ESPIPE */
#ifdef ESRCH
  { 123, ESRCH },
#endif /* ESRCH */
#ifdef ESRMNT
  { 124, ESRMNT },
#endif /* ESRMNT */
#ifdef ESTALE
  { 125, ESTALE },
#endif /* ESTALE */
#ifdef ESUCCESS
  { 126, ESUCCESS },
#endif /* ESUCCESS */
#ifdef ETIME
  { 127, ETIME },
#endif /* ETIME */
#ifdef ETIMEDOUT
  { 128, ETIMEDOUT },
#endif /* ETIMEDOUT */
#ifdef ETOOMANYREFS
  { 129, ETOOMANYREFS },
#endif /* ETOOMANYREFS */
#ifdef ETXTBSY
  { 130, ETXTBSY },
#endif /* ETXTBSY */
#ifdef EUCLEAN
  { 131, EUCLEAN },
#endif /* EUCLEAN */
#ifdef EUNATCH
  { 132, EUNATCH },
#endif /* EUNATCH */
#ifdef EUSERS
  { 133, EUSERS },
#endif /* EUSERS */
#ifdef EVERSION
  { 134, EVERSION },
#endif /* EVERSION */
#ifdef EWOULDBLOCK
  { 135, EWOULDBLOCK },
#endif /* EWOULDBLOCK */
#ifdef EXDEV
  { 136, EXDEV },
#endif /* EXDEV */
#ifdef EXFULL
  { 137, EXFULL },
#endif /* EXFULL */
#ifdef NXDOMAIN
  { 138, NXDOMAIN },
#endif /* NXDOMAIN */
  { 139, 0 }
};
static const size_t n_errno_map = sizeof (errno_map) / sizeof (errno_map[0]);

static int
errno_map_compar (const void *a,
                  const void *b)
{
  const ErrnoMap *map_a = a;
  const ErrnoMap *map_b = b;

  return (map_a->canonical < map_b->canonical) ? -1 :
         (map_a->canonical > map_b->canonical) ? 1 : 0;
}

/*=====================================================================*
 *                                Public                               *
 *=====================================================================*/

int
fuserl_errno_decanonicalize     (int canonical)
{
  ErrnoMap key = { .canonical = canonical };
  ErrnoMap *value = bsearch (&key,
                             errno_map,
                             n_errno_map,
                             sizeof (errno_map[0]),
                             errno_map_compar);

  return (value != NULL) ? value->native : EINVAL;
}

int
fuserl_access_mode_canonicalize (int mode)
{
  int canonical = 0;

                                                /* F_OK -> 0 */
  if ((mode & X_OK) == X_OK) { canonical |= 1; }  /* X_OK -> 1 */
  if ((mode & W_OK) == W_OK) { canonical |= 2; }  /* W_OK -> 2 */
  if ((mode & R_OK) == R_OK) { canonical |= 4; }  /* R_OK -> 4 */

  return canonical;
}

int
fuserl_attr_flags_canonicalize (int flags)
{
  int canonical = 0;

  (void) flags;

#if HAVE_SETXATTR
  if ((flags & XATTR_CREATE) == XATTR_CREATE) { canonical |= 1; } /* XATTR_CREATE -> 1 */
  if ((flags & XATTR_REPLACE) == XATTR_REPLACE) { canonical |= 2; } /* XATTR_REPLACE -> 2 */
#endif

  return canonical;
}

int
fuserl_lock_cmd_canonicalize (int cmd)
{
  switch (cmd)
    {
      case F_GETLK:
        return 0;
      case F_SETLK:
        return 1;
      case F_SETLKW:
        return 2;
      default:
        return 3;
    }
}

short
fuserl_l_type_canonicalize (short l_type)
{
  switch (l_type)
    {
      case F_RDLCK:
        return 0;
      case F_WRLCK:
        return 1;
      case F_UNLCK:
        return 2;
      default:
        return 3;
    }
}

short
fuserl_l_whence_canonicalize (short l_whence)
{
  switch (l_whence)
    {
      case SEEK_SET:
        return 0;
      case SEEK_CUR:
        return 1;
      case SEEK_END:
        return 2;
      default:
        return 3;
    }
}

int
fuserl_open_flags_canonicalize (int flags)
{
  int canonical = 0;

  if ((flags & O_ACCMODE) == O_RDONLY) {      canonical |= 00000000; }
  if ((flags & O_ACCMODE) == O_WRONLY) {      canonical |= 00000001; }
  if ((flags & O_ACCMODE) == O_RDWR) {        canonical |= 00000002; }
  if ((flags & O_CREAT) == O_CREAT) {       canonical |= 00000100; }
  if ((flags & O_EXCL) == O_EXCL) {        canonical |= 00000200; }
  if ((flags & O_NOCTTY) == O_NOCTTY) {      canonical |= 00000400; }
  if ((flags & O_TRUNC) == O_TRUNC) {       canonical |= 00001000; }
  if ((flags & O_APPEND) == O_APPEND) {      canonical |= 00002000; }
  if ((flags & O_NONBLOCK) == O_NONBLOCK) {    canonical |= 00004000; }
  if ((flags & O_NDELAY) == O_NDELAY) {      canonical |= 00004000; }
  if ((flags & O_SYNC) == O_SYNC) {        canonical |= 00010000; }
  if ((flags & FASYNC) == FASYNC) {        canonical |= 00020000; }
#ifdef O_DIRECT
  if ((flags & O_DIRECT) == O_DIRECT) {      canonical |= 00040000; }
#endif
#ifdef O_LARGEFILE
  if ((flags & O_LARGEFILE) == O_LARGEFILE) {   canonical |= 00100000; }
#endif
#ifdef O_DIRECTORY
  if ((flags & O_DIRECTORY) == O_DIRECTORY) {   canonical |= 00200000; }
#endif
#ifdef O_NOFOLLOW
  if ((flags & O_NOFOLLOW) == O_NOFOLLOW) {    canonical |= 00400000; }
#endif
#ifdef O_NOATIME
  if ((flags & O_NOATIME) == O_NOATIME) {     canonical |= 01000000; }
#endif

  return canonical;
}

int
fuserl_open_flags_decanonicalize (int canonical)
{
  int flags = 0;

  if ((canonical & 00000003) == 00000000) { flags |= O_RDONLY; }
  if ((canonical & 00000003) == 00000001) { flags |= O_WRONLY; }
  if ((canonical & 00000003) == 00000002) { flags |= O_RDWR; }
  if ((canonical & 00000100) == 00000100) { flags |= O_CREAT; }
  if ((canonical & 00000200) == 00000200) { flags |= O_EXCL; }
  if ((canonical & 00000400) == 00000400) { flags |= O_NOCTTY; }
  if ((canonical & 00001000) == 00001000) { flags |= O_TRUNC; }
  if ((canonical & 00002000) == 00002000) { flags |= O_APPEND; }
  if ((canonical & 00004000) == 00004000) { flags |= O_NONBLOCK; }
  if ((canonical & 00004000) == 00004000) { flags |= O_NDELAY; }
  if ((canonical & 00010000) == 00010000) { flags |= O_SYNC; }
  if ((canonical & 00020000) == 00020000) { flags |= FASYNC; }
#ifdef O_DIRECT
  if ((canonical & 00040000) == 00040000) { flags |= O_DIRECT; }
#endif
#ifdef O_LARGEFILE
  if ((canonical & 00100000) == 00100000) { flags |= O_LARGEFILE; }
#endif
#ifdef O_DIRECTORY
  if ((canonical & 00200000) == 00200000) { flags |= O_DIRECTORY; }
#endif
#ifdef O_NOFOLLOW
  if ((canonical & 00400000) == 00400000) { flags |= O_NOFOLLOW; }
#endif
#ifdef O_NOATIME
  if ((canonical & 01000000) == 01000000) { flags |= O_NOATIME; }
#endif

  return flags;
}

int
fuserl_stat_mode_canonicalize (mode_t mode)
{
  int canonical = 0;

  if ((mode & S_IFSOCK) == S_IFSOCK) { canonical |= 0140000; }  
  if ((mode & S_IFLNK) == S_IFLNK) {  canonical |= 0120000; }  
  if ((mode & S_IFREG) == S_IFREG) {  canonical |= 0100000; }  
  if ((mode & S_IFBLK) == S_IFBLK) {  canonical |= 0060000; }  
  if ((mode & S_IFDIR) == S_IFDIR) {  canonical |= 0040000; }  
  if ((mode & S_IFCHR) == S_IFCHR) {  canonical |= 0020000; }  
  if ((mode & S_IFIFO) == S_IFIFO) {  canonical |= 0010000; }  
  if ((mode & S_ISUID) == S_ISUID) {  canonical |= 0004000; }  
  if ((mode & S_ISGID) == S_ISGID) {  canonical |= 0002000; }  
  if ((mode & S_ISVTX) == S_ISVTX) {  canonical |= 0001000; }  
  if ((mode & S_IRUSR) == S_IRUSR) {  canonical |= 00400; }    
  if ((mode & S_IWUSR) == S_IWUSR) {  canonical |= 00200; }    
  if ((mode & S_IXUSR) == S_IXUSR) {  canonical |= 00100; }    
  if ((mode & S_IRGRP) == S_IRGRP) {  canonical |= 00040; }    
  if ((mode & S_IWGRP) == S_IWGRP) {  canonical |= 00020; }    
  if ((mode & S_IXGRP) == S_IXGRP) {  canonical |= 00010; }    
  if ((mode & S_IROTH) == S_IROTH) {  canonical |= 00004; }    
  if ((mode & S_IWOTH) == S_IWOTH) {  canonical |= 00002; }    
  if ((mode & S_IXOTH) == S_IXOTH) {  canonical |= 00001; }    

  return canonical;
}

short
fuserl_l_type_decanonicalize (short l_type)
{
  switch (l_type)
    {
      case 0:
        return F_RDLCK;
      case 1:
        return F_WRLCK;
      case 2:
        return F_UNLCK;
      default:
        return -1;
    }
}

short
fuserl_l_whence_decanonicalize (short l_whence)
{
  switch (l_whence)
    {
      case 0:
        return SEEK_SET;
      case 1:
        return SEEK_CUR;
      case 2:
        return SEEK_END;
      default:
        return -1;
    }
}

mode_t
fuserl_stat_mode_decanonicalize (int canonical)
{
  mode_t mode = 0;

  if ((canonical & 0140000) == 0140000) { mode |= S_IFSOCK; }  
  if ((canonical & 0120000) == 0120000) { mode |= S_IFLNK; }  
  if ((canonical & 0100000) == 0100000) { mode |= S_IFREG; }  
  if ((canonical & 0060000) == 0060000) { mode |= S_IFBLK; }  
  if ((canonical & 0040000) == 0040000) { mode |= S_IFDIR; }  
  if ((canonical & 0020000) == 0020000) { mode |= S_IFCHR; }  
  if ((canonical & 0010000) == 0010000) { mode |= S_IFIFO; }  
  if ((canonical & 0004000) == 0004000) { mode |= S_ISUID; }  
  if ((canonical & 0002000) == 0002000) { mode |= S_ISGID; }  
  if ((canonical & 0001000) == 0001000) { mode |= S_ISVTX; }  
  if ((canonical & 00400) == 00400) { mode |= S_IRUSR; }    
  if ((canonical & 00200) == 00200) { mode |= S_IWUSR; }    
  if ((canonical & 00100) == 00100) { mode |= S_IXUSR; }    
  if ((canonical & 00040) == 00040) { mode |= S_IRGRP; }    
  if ((canonical & 00020) == 00020) { mode |= S_IWGRP; }    
  if ((canonical & 00010) == 00010) { mode |= S_IXGRP; }    
  if ((canonical & 00004) == 00004) { mode |= S_IROTH; }    
  if ((canonical & 00002) == 00002) { mode |= S_IWOTH; }    
  if ((canonical & 00001) == 00001) { mode |= S_IXOTH; }    

  return mode;
}
