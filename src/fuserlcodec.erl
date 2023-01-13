%% @doc Protocol for talking to the driver.
%% @hidden
%% @end

-module (fuserlcodec).

-export ([ decode_ffi/1,
           decode_ffi_ptr/1,
           decode_flock/1,
           decode_stat/1,
           encode_uint8/1,
           encode_direntry_list/1,
           encode_flock/1,
           encode_fuse_entry_param/1,
           encode_fuse_file_info/1,
           encode_int64/1,
           encode_uint64/1,
           encode_pointer/1,
           encode_stat/1,
           encode_statvfs/1,
           encode_string/1 ]).

-export([encode_opcode/1]).
-export([decode_opcode/1]).

-include ("../include/fuserl.hrl").

-define(KV(K,V), (K) => (V), (V) => (K)).

flock_type_kv() ->
    #{ ?KV(f_rdlck, 0),
       ?KV(f_wrlck, 1),
       ?KV(f_unlck, 2) }.

flock_whence_kv() ->
    #{ ?KV(seek_set, 0),
       ?KV(seek_cur, 1),
       ?KV(seek_end, 2) }.

%-=====================================================================-
%-                                Public                               -
%-=====================================================================-

encode_opcode (lookup) -> ?FUSERL_LL_LOOKUP;
encode_opcode (forget) -> ?FUSERL_LL_FORGET;
encode_opcode (getattr) -> ?FUSERL_LL_GETATTR;
encode_opcode (setattr) -> ?FUSERL_LL_SETATTR;
encode_opcode (readlink) -> ?FUSERL_LL_READLINK;
encode_opcode (mknod) -> ?FUSERL_LL_MKNOD;
encode_opcode (mkdir) -> ?FUSERL_LL_MKDIR;
encode_opcode (unlink) -> ?FUSERL_LL_UNLINK;
encode_opcode (rmdir) -> ?FUSERL_LL_RMDIR;
encode_opcode (symlink) -> ?FUSERL_LL_SYMLINK;
encode_opcode (rename) -> ?FUSERL_LL_RENAME;
encode_opcode (link) -> ?FUSERL_LL_LINK;
encode_opcode (open) -> ?FUSERL_LL_OPEN;
encode_opcode (read) -> ?FUSERL_LL_READ;
encode_opcode (write) -> ?FUSERL_LL_WRITE;
encode_opcode (flush) -> ?FUSERL_LL_FLUSH;
encode_opcode (release) -> ?FUSERL_LL_RELEASE;
encode_opcode (fsync) -> ?FUSERL_LL_FSYNC;
encode_opcode (opendir) -> ?FUSERL_LL_OPENDIR;
encode_opcode (readdir) -> ?FUSERL_LL_READDIR;
encode_opcode (releasedir) -> ?FUSERL_LL_RELEASEDIR;
encode_opcode (fsyncdir) -> ?FUSERL_LL_FSYNCDIR;
encode_opcode (statfs) -> ?FUSERL_LL_STATFS;
encode_opcode (setxattr) -> ?FUSERL_LL_SETXATTR;
encode_opcode (getxattr) -> ?FUSERL_LL_GETXATTR;
encode_opcode (listxattr) -> ?FUSERL_LL_LISTXATTR;
encode_opcode (removexattr) -> ?FUSERL_LL_REMOVEXATTR;
encode_opcode (access) -> ?FUSERL_LL_ACCESS;
encode_opcode (create) -> ?FUSERL_LL_CREATE;
encode_opcode (getlk) -> ?FUSERL_LL_GETLK;
encode_opcode (setlk) -> ?FUSERL_LL_SETLK;
encode_opcode (bmap) -> ?FUSERL_LL_BMAP.


decode_opcode (?FUSERL_LL_LOOKUP) -> lookup;
decode_opcode (?FUSERL_LL_FORGET) -> forget;
decode_opcode (?FUSERL_LL_GETATTR) -> getattr;
decode_opcode (?FUSERL_LL_SETATTR) -> setattr;
decode_opcode (?FUSERL_LL_READLINK) -> readlink;
decode_opcode (?FUSERL_LL_MKNOD) -> mknod;
decode_opcode (?FUSERL_LL_MKDIR) -> mkdir;
decode_opcode (?FUSERL_LL_UNLINK) -> unlink;
decode_opcode (?FUSERL_LL_RMDIR) -> rmdir;
decode_opcode (?FUSERL_LL_SYMLINK) -> symlink;
decode_opcode (?FUSERL_LL_RENAME) -> rename;
decode_opcode (?FUSERL_LL_LINK) -> link;
decode_opcode (?FUSERL_LL_OPEN) -> open;
decode_opcode (?FUSERL_LL_READ) -> read;
decode_opcode (?FUSERL_LL_WRITE) -> write;
decode_opcode (?FUSERL_LL_FLUSH) -> flush;
decode_opcode (?FUSERL_LL_RELEASE) -> release;
decode_opcode (?FUSERL_LL_FSYNC) -> fsync;
decode_opcode (?FUSERL_LL_OPENDIR) -> opendir;
decode_opcode (?FUSERL_LL_READDIR) -> readdir;
decode_opcode (?FUSERL_LL_RELEASEDIR) -> releasedir;
decode_opcode (?FUSERL_LL_FSYNCDIR) -> fsyncdir;
decode_opcode (?FUSERL_LL_STATFS) -> statfs;
decode_opcode (?FUSERL_LL_SETXATTR) -> setxattr;
decode_opcode (?FUSERL_LL_GETXATTR) -> getxattr;
decode_opcode (?FUSERL_LL_LISTXATTR) -> listxattr;
decode_opcode (?FUSERL_LL_REMOVEXATTR) -> removexattr;
decode_opcode (?FUSERL_LL_ACCESS) -> access;
decode_opcode (?FUSERL_LL_CREATE) -> create;
decode_opcode (?FUSERL_LL_GETLK) -> getlk;
decode_opcode (?FUSERL_LL_SETLK) -> setlk;
decode_opcode (?FUSERL_LL_BMAP) -> bmap.

decode_ffi (<<?int64(Flags),
              ?uint8(WritePage),
              ?uint8(DirectIo),
              ?uint8(KeepCache),
              ?uint8(Flush),
              ?uint64(Fh),
              ?uint64(LockOwner),
              ?bin(Rest)>>) ->
  { #fuse_file_info{ flags = Flags,
                     writepage = (WritePage =/= 0),
                     direct_io = (DirectIo =/= 0),
                     keep_cache = (KeepCache =/= 0),
                     flush = (Flush =/= 0),
                     fh = Fh,
                     lock_owner = LockOwner },
    Rest }.

decode_ffi_ptr (<<Null:8, Rest/binary>>) ->
  case Null of
    0 ->
      decode_ffi (Rest);
    1 ->
      { null, Rest }
  end.

decode_flock (<<?uint8(Type),
                ?uint8(Whence),
                ?uint64(Start),
                ?uint64(Len),
                ?uint64(Pid),
                Rest/binary>>) ->
  %% FlockType = case Type of 0 -> f_rdlck; 1 -> f_wrlck; 2 -> f_unlck end,
  %% FlockWhence = case Whence of 0 -> seek_set; 1 -> seek_cur; 2 -> seek_end end,
  { #flock{ l_type = maps:get(Type, flock_type_kv()),
            l_whence = maps:get(Whence, flock_whence_kv()),
            l_start = Start,
            l_len = Len,
            l_pid = Pid }, Rest }.

decode_stat (<<?uint64(StDevMajor),
               ?uint64(StDevMinor),
               ?uint64(StIno),
               ?uint64(StMode),
               ?uint64(StNLink),
               ?int64(StUid),
	       ?int64(StGid),
               ?uint64(StRDevMajor),
               ?uint64(StRDevMinor),
               ?uint64(StSize),
               ?uint64(StBlkSize),
               ?uint64(StBlocks),
               ?uint64(StATime),
               ?uint64(StMTime),
               ?uint64(StCTime),
               ?bin(Rest)>>) ->
  { #stat{ st_dev = { StDevMajor, StDevMinor },
           st_ino = StIno,
           st_mode = StMode,
           st_nlink = StNLink,
           st_uid = StUid,
           st_gid = StGid,
           st_rdev = { StRDevMajor, StRDevMinor },
           st_size = StSize,
           st_blksize = StBlkSize,
           st_blocks = StBlocks,
           st_atime = StATime,
           st_mtime = StMTime,
           st_ctime = StCTime }, 
    Rest }.

encode_uint8 (X) when is_integer (X), X >= 0, X =< 255 -> <<?uint8(X)>>.
encode_int64 (X) when is_integer (X) -> << ?int64(X) >>.
encode_uint64 (X) when is_integer (X) -> << ?uint64(X) >>.
encode_pointer (X) when is_integer (X) -> << ?uint64(X) >>.

encode_direntry_list (List) ->
    [ encode_dirent (E) || E <- List ].

encode_dirent(E = #direntry{}) ->
    << ?bin(encode_string (E#direntry.name)),
       ?uint64 (E#direntry.offset),
       ?bin(encode_stat (E#direntry.stat)) >>.


encode_flock (Flock = #flock{}) ->
  Type = maps:get(Flock#flock.l_type, flock_type_kv()),
    %% case Flock#flock.l_type of f_rdlck -> 0; f_wrlck -> 1; f_unlck -> 2 end,
  Whence = maps:get(Flock#flock.l_whence, flock_whence_kv()),
%%    case Flock#flock.l_whence of seek_set -> 0; seek_cur -> 1; seek_end -> 2 end,
  <<?uint8(Type),
    ?uint8(Whence),
    ?uint64(Flock#flock.l_start),
    ?uint64(Flock#flock.l_len),
    ?uint64(Flock#flock.l_pid)>>.

encode_fuse_entry_param (Entry = #fuse_entry_param{}) ->
 << ?uint64 (Entry#fuse_entry_param.ino),
    ?uint64 (Entry#fuse_entry_param.generation), 
    ?bin (encode_stat (Entry#fuse_entry_param.attr)),
    ?uint64 (Entry#fuse_entry_param.attr_timeout_ms),
    ?uint64 (Entry#fuse_entry_param.entry_timeout_ms)
  >>.

encode_fuse_file_info (Fi = #fuse_file_info{}) ->
    << ?uint64 (Fi#fuse_file_info.flags),
       ?enc_bool (Fi#fuse_file_info.writepage),
       ?enc_bool (Fi#fuse_file_info.direct_io),
       ?enc_bool (Fi#fuse_file_info.keep_cache),
       ?enc_bool (Fi#fuse_file_info.flush),
       ?uint64 (Fi#fuse_file_info.fh),
       ?uint64 (Fi#fuse_file_info.lock_owner) >>.


encode_stat (null) ->
    << ?false >>;
encode_stat (Stat = #stat{}) ->
    << ?true,
       ?uint64 (erlang:element (1, Stat#stat.st_dev)),
       ?uint64 (erlang:element (2, Stat#stat.st_dev)),
       ?uint64 (Stat#stat.st_ino),
       ?uint64 (Stat#stat.st_mode),
       ?uint64 (Stat#stat.st_nlink),
       ?int64 (Stat#stat.st_uid),
       ?int64 (Stat#stat.st_gid),
       ?uint64 (erlang:element (1, Stat#stat.st_rdev)),
       ?uint64 (erlang:element (2, Stat#stat.st_rdev)),
       ?uint64 (Stat#stat.st_size),
       ?uint64 (Stat#stat.st_blksize),
       ?uint64 (Stat#stat.st_blocks),
       ?uint64 (Stat#stat.st_atime),
       ?uint64 (Stat#stat.st_mtime),
       ?uint64 (Stat#stat.st_ctime) >>.

encode_statvfs (StatVFS = #statvfs{}) ->
    << ?uint64 (StatVFS#statvfs.f_bsize),
       ?uint64 (StatVFS#statvfs.f_frsize),
       ?uint64 (StatVFS#statvfs.f_blocks),
       ?uint64 (StatVFS#statvfs.f_bfree),
       ?uint64 (StatVFS#statvfs.f_bavail),
       ?uint64 (StatVFS#statvfs.f_files),
       ?uint64 (StatVFS#statvfs.f_ffree),
       ?uint64 (StatVFS#statvfs.f_favail),
       ?uint64 (StatVFS#statvfs.f_fsid), 
       ?uint64 (StatVFS#statvfs.f_flag), 
       ?uint64 (StatVFS#statvfs.f_namemax) >>.

encode_string (S) ->
    Bin = iolist_to_binary(S),
    Len = 1 + byte_size(Bin),
    <<?uint64(Len), Bin/binary, 0:8>>.
