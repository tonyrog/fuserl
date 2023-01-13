-ifndef (FUSERL_HRL).
-define (FUSERL_HRL, true).

%-=====================================================================-
%-                            access macros                            -
%-=====================================================================-

% These are portable: the C driver canonicalizes from the local mask values.

-define (F_OK, 0).               % Test for existence.  
-define (X_OK, 1).               % Test for execute permission.  
-define (W_OK, 2).               % Test for write permission.  
-define (R_OK, 4).               % Test for read permission.  

%-=====================================================================-
%-                             stat macros                             -
%-=====================================================================-

% These are compatible with file:read_file_info/1 et. al.

-define (S_IFMT,   8#0170000).  % bitmask for the file type bitfields
-define (S_IFSOCK, 8#0140000).  % socket
-define (S_IFLNK,  8#0120000).  % symbolic link
-define (S_IFREG,  8#0100000).  % regular file
-define (S_IFBLK,  8#0060000).  % block device
-define (S_IFDIR,  8#0040000).  % directory
-define (S_IFCHR,  8#0020000).  % character device
-define (S_IFIFO,  8#0010000).  % FIFO
-define (S_ISUID,  8#0004000).  % set UID bit
-define (S_ISGID,  8#0002000).  % set-group-ID bit (see below)
-define (S_ISVTX,  8#0001000).  % sticky bit (see below)
-define (S_IRWXU,  8#00700).    % mask for file owner permissions
-define (S_IRUSR,  8#00400).    % owner has read permission
-define (S_IWUSR,  8#00200).    % owner has write permission
-define (S_IXUSR,  8#00100).    % owner has execute permission
-define (S_IRWXG,  8#00070).    % mask for group permissions
-define (S_IRGRP,  8#00040).    % group has read permission
-define (S_IWGRP,  8#00020).    % group has write permission
-define (S_IXGRP,  8#00010).    % group has execute permission
-define (S_IRWXO,  8#00007).    % mask for permissions for others (not in group)
-define (S_IROTH,  8#00004).    % others have read permission
-define (S_IWOTH,  8#00002).    % others have write permission
-define (S_IXOTH,  8#00001).    % others have execute permission

% These are the bitmask values for the ToSet argument to setattr.

-define (FUSE_SET_ATTR_MODE, (1 bsl 0)).
-define (FUSE_SET_ATTR_UID, (1 bsl 1)).
-define (FUSE_SET_ATTR_GID, (1 bsl 2)).
-define (FUSE_SET_ATTR_SIZE, (1 bsl 3)).
-define (FUSE_SET_ATTR_ATIME, (1 bsl 4)).
-define (FUSE_SET_ATTR_MTIME, (1 bsl 5)).

%-=====================================================================-
%-                             open macros                             -
%-=====================================================================-

% These are portable: the C driver canonicalizes from the local mask values.

-define (O_ACCMODE,     8#00000003).
-define (O_RDONLY,      8#00000000).
-define (O_WRONLY,      8#00000001).
-define (O_RDWR,        8#00000002).
-define (O_CREAT,       8#00000100).        % not fcntl 
-define (O_EXCL,        8#00000200).        % not fcntl 
-define (O_NOCTTY,      8#00000400).        % not fcntl 
-define (O_TRUNC,       8#00001000).        % not fcntl 
-define (O_APPEND,      8#00002000).
-define (O_NONBLOCK,    8#00004000).
-define (O_NDELAY,      ?O_NONBLOCK).
-define (O_SYNC,        8#00010000).
-define (FASYNC,        8#00020000).        % fcntl, for BSD compatibility 

% NB: O_DIRECT, O_LARGEFILE, O_DIRECTORY, O_NOFOLLOW, and O_NOATIME
% might not be supported on all architectures

-define (O_DIRECT,      8#00040000).        % direct disk access hint 
-define (O_LARGEFILE,   8#00100000).
-define (O_DIRECTORY,   8#00200000).        % must be a directory 
-define (O_NOFOLLOW,    8#00400000).        % don't follow links 
-define (O_NOATIME,     8#01000000).

%-=====================================================================-
%-                             xattr macros                            -
%-=====================================================================-

-define (XATTR_CREATE,   1).    % set value, fail if attr already exists.
-define (XATTR_REPLACE,  2).    % set value, fail if attr does not exist.

%-=====================================================================-
%-                               records                               -
%-=====================================================================-

% Got this from stat(2).  Devices numbers are { major, minor }.

-record (stat, { st_dev = { 0, 0 },     % ID of device containing file 
                 st_ino = 0,            % inode number 
                 st_mode = 0,           % protection 
                 st_nlink = 0,          % number of hard links 
                 st_uid = 0,            % user ID of owner 
                 st_gid = 0,            % group ID of owner 
                 st_rdev = { 0, 0 },    % device ID (if special file) 
                 st_size = 0,           % total size = 0, in bytes 
                 st_blksize = 0,        % blocksize for filesystem I/O 
                 st_blocks = 0,         % number of blocks allocated 
                 st_atime = 0,          % time of last access 
                 st_mtime = 0,          % time of last modification 
                 st_ctime = 0           % time of last status change 
               }).

% Got this from statvfs(2).

-record (statvfs, { f_bsize = 0,    % file system block size 
                    f_frsize = 0,   % fragment size 
                    f_blocks = 0,   % size of fs in f_frsize units 
                    f_bfree = 0,    % # free blocks 
                    f_bavail = 0,   % # free blocks for non-root 
                    f_files = 0,    % # inodes 
                    f_ffree = 0,    % # free inodes 
                    f_favail = 0,   % # free inodes for non-root 
                    f_fsid = 0,     % file system ID 
                    f_flag = 0,     % mount flags 
                    f_namemax = 0   % maximum filename length 
                  }).

% Got this from fcntl(2).

-record (flock, { l_type=f_unlck,    % Type of lock: f_rdlck, f_wrlck, f_unlck 
                  l_whence=seek_set, % How to interpret l_start:
                                     %  seek_set | seek_cur | seek_end
                  l_start= 0,        % Starting offset for lock 
                  l_len= 0,          % Number of bytes to lock 
                  l_pid= 0           % PID of process blocking our lock
                                     %   (F_GETLK only) 
                }).

% Made this one up, so should be ok.

-record (direntry, { name,
                     offset,
                     stat }).

% Got this from fuse_lowlevel.h.

-record (fuse_entry_param, { 
          %% Unique inode number
           %
           % In lookup, zero means negative entry (from version 2.5)
           % Returning ENOENT also means negative entry, but by setting zero
           % ino the kernel may cache negative entries for entry_timeout
           % seconds.
           
          ino,                            % non-negative integer
  
          %% Generation number for this entry.
           %
           % The ino/generation pair should be unique for the filesystem's
           % lifetime. It must be non-zero, otherwise FUSE will treat it as an
           % error.
           
          generation,                     % positive integer
  
          %% Inode attributes.
           %
           % example, % Even if attr_timeout == 0, attr must be correct. For
           % for open(), FUSE uses attr.st_size from lookup() to determine
           % how many bytes to request. If this value is not
           % incorrect data will be returned.
           
          attr,                           % stat{}
  
          %% Validity timeout (in milliseconds) for the attributes 
          attr_timeout_ms,                % non-negative integer
  
          %% Validity timeout (in milliseconds) for the name 
          entry_timeout_ms                % non-negative integer
        }).

% Got this from fuse_common.h, minus the crap that you are not supposed
% to use anymore.  Should be ok until fuse changes it.

-record (fuse_file_info, { 
           % Open flags.  Available in open() and release() 
           flags,

           % In case of a write operation indicates if this was caused by a
           % writepage 
           writepage,

           % Can be filled in by open, to use direct I/O on this file.
           % Introduced in version 2.4 
           direct_io,

           % Can be filled in by open, to indicate, that cached file data
           %    need not be invalidated.  Introduced in version 2.4 
           keep_cache,

           % Indicates a flush operation.  Set in flush operation, also
           %    maybe set in highlevel lock operation and lowlevel release
           %    operation.  Introduced in version 2.6 
           flush,

           % File handle.  May be filled in by filesystem in open().
           %    Available in all other file operations 
           fh,

           % Lock owner id.  Available in locking operations and flush 
           lock_owner
        }).

% Additional context associated with requests
-record (fuse_ctx, {
           % User ID of the calling process 
           uid,
         
           % Group ID of the calling process
           gid,
         
           % Thread ID of the calling process 
           pid
         }).

%-=====================================================================-
%-                 fuse_reply types (for lowlevel API)                 -
%-=====================================================================-

-define (FUSERL_LL_LOOKUP, 0).
-define (FUSERL_LL_FORGET, 1).
-define (FUSERL_LL_GETATTR, 2).
-define (FUSERL_LL_SETATTR, 3).
-define (FUSERL_LL_READLINK, 4).
-define (FUSERL_LL_MKNOD, 5).
-define (FUSERL_LL_MKDIR, 6).
-define (FUSERL_LL_UNLINK, 7).
-define (FUSERL_LL_RMDIR, 8).
-define (FUSERL_LL_SYMLINK, 9).
-define (FUSERL_LL_RENAME, 10).
-define (FUSERL_LL_LINK, 11).
-define (FUSERL_LL_OPEN, 12).
-define (FUSERL_LL_READ, 13).
-define (FUSERL_LL_WRITE, 14).
-define (FUSERL_LL_FLUSH, 15).
-define (FUSERL_LL_RELEASE, 16).
-define (FUSERL_LL_FSYNC, 17).
-define (FUSERL_LL_OPENDIR, 18).
-define (FUSERL_LL_READDIR, 19).
-define (FUSERL_LL_RELEASEDIR, 20).
-define (FUSERL_LL_FSYNCDIR, 21).
-define (FUSERL_LL_STATFS, 22).
-define (FUSERL_LL_SETXATTR, 23).
-define (FUSERL_LL_GETXATTR, 24).
-define (FUSERL_LL_LISTXATTR, 25).
-define (FUSERL_LL_REMOVEXATTR, 26).
-define (FUSERL_LL_ACCESS, 27).
-define (FUSERL_LL_CREATE, 28).
-define (FUSERL_LL_GETLK, 29).
-define (FUSERL_LL_SETLK, 30).
-define (FUSERL_LL_BMAP, 31).

% wrt fuse_reply_err, some methods respond with #fuse_reply_err{err = ok}
% to indicate success, e.g., rmdir.
-record (fuse_reply_err, { err }).
-record (fuse_reply_none, { }).
-record (fuse_reply_entry, { fuse_entry_param }).
-record (fuse_reply_create, { fuse_entry_param, fuse_file_info }).
-record (fuse_reply_attr, { attr, attr_timeout_ms }).
-record (fuse_reply_readlink, { link }).
-record (fuse_reply_open, { fuse_file_info }).
-record (fuse_reply_write, { count }).
-record (fuse_reply_buf, { buf, size }). % buf is type io_list ()
-record (fuse_reply_statfs, { statvfs }).
-record (fuse_reply_xattr, { count }).
-record (fuse_reply_lock, { flock }).
-record (fuse_reply_direntrylist, { direntrylist }).


-define(uint8(X),  (X):8).
-define(int64(X),  (X):64/native-signed).
-define(uint64(X), (X):64/native-unsigned).
-define(bin(X),    (X)/binary).
-define(bin(X,L),  (X):(L)/binary).
-define(true,      (1):8).
-define(false,     (0):8).

-define(enc_bool(X), (if (X)->1; true -> 0 end)).

-endif.
