%% @doc Protocol for talking to the driver.
%% @hidden
%% @end

-module (fuserlcodec).

-export ([ decode_ffi/1,
           decode_ffi_ptr/1,
           decode_flock/1,
           decode_stat/1,
           encode_binary/2,
           encode_byte/1,
           encode_direntry_list/1,
           encode_flock/1,
           encode_fuse_entry_param/1,
           encode_fuse_file_info/1,
           encode_native_64_signed/1,
           encode_native_64_unsigned/1,
           encode_pointer/1,
           encode_stat/1,
           encode_statvfs/1,
           encode_string/1 ]).

-include ("../include/fuserl.hrl").

%-=====================================================================-
%-                                Public                               -
%-=====================================================================-

decode_ffi (<<Flags:64/native-signed,
              WritePage:8,
              DirectIo:8,
              KeepCache:8,
              Flush:8,
              Fh:64/native-unsigned,
              LockOwner:64/native-unsigned,
              Rest/binary>>) ->
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

decode_flock (<<Type:8,
                Whence:8,
                Start:64/native-unsigned,
                Len:64/native-unsigned,
                Pid:64/native-unsigned,
                Rest/binary>>) ->
  FlockType = case Type of 0 -> f_rdlck; 1 -> f_wrlck; 2 -> f_unlck end,
  FlockWhence = case Whence of 0 -> seek_set; 1 -> seek_cur; 2 -> seek_end end,

  { #flock{ l_type = FlockType,
            l_whence = FlockWhence,
            l_start = Start,
            l_len = Len,
            l_pid = Pid }, Rest }.

decode_stat (<<StDevMajor:64/native-unsigned,
               StDevMinor:64/native-unsigned,
               StIno:64/native-unsigned,
               StMode:64/native-unsigned,
               StNLink:64/native-unsigned,
               StUid:64/native-signed,
               StGid:64/native-signed,
               StRDevMajor:64/native-unsigned,
               StRDevMinor:64/native-unsigned,
               StSize:64/native-unsigned,
               StBlkSize:64/native-unsigned,
               StBlocks:64/native-unsigned,
               StATime:64/native-unsigned,
               StMTime:64/native-unsigned,
               StCTime:64/native-unsigned,
               Rest/binary>>) ->
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

encode_binary (Size, IoList) ->
  [ encode_native_64_unsigned (Size),
    IoList ].

encode_byte (true) -> <<1:8>>;
encode_byte (false) -> <<0:8>>;
encode_byte (X) when is_integer (X), X >= 0, X =< 127 -> 
  <<X:8>>.

encode_direntry_list ([]) -> 
  <<>>;
encode_direntry_list ([ DirEntry = #direntry{} | Rest ]) ->
  [ encode_string (DirEntry#direntry.name),
    encode_native_64_unsigned (DirEntry#direntry.offset),
    encode_stat (DirEntry#direntry.stat),
    encode_direntry_list (Rest) ].

encode_flock (Flock = #flock{}) ->
  Type = 
    case Flock#flock.l_type of f_rdlck -> 0; f_wrlck -> 1; f_unlck -> 2 end,
  Whence = 
    case Flock#flock.l_whence of seek_set -> 0; seek_cur -> 1; seek_end -> 2 end,

  <<Type:8,
    Whence:8,
    (Flock#flock.l_start):64/native-unsigned,
    (Flock#flock.l_len):64/native-unsigned,
    (Flock#flock.l_pid):64/native-unsigned>>.

encode_fuse_entry_param (Entry = #fuse_entry_param{}) ->
  [ encode_native_64_unsigned (Entry#fuse_entry_param.ino),
    encode_native_64_unsigned (Entry#fuse_entry_param.generation), 
    encode_stat (Entry#fuse_entry_param.attr),
    encode_native_64_unsigned (Entry#fuse_entry_param.attr_timeout_ms),
    encode_native_64_unsigned (Entry#fuse_entry_param.entry_timeout_ms)
  ].

encode_fuse_file_info (Fi = #fuse_file_info{}) ->
  [ encode_native_64_unsigned (Fi#fuse_file_info.flags),
    encode_byte (Fi#fuse_file_info.writepage),
    encode_byte (Fi#fuse_file_info.direct_io),
    encode_byte (Fi#fuse_file_info.keep_cache),
    encode_byte (Fi#fuse_file_info.flush),
    encode_native_64_unsigned (Fi#fuse_file_info.fh),
    encode_native_64_unsigned (Fi#fuse_file_info.lock_owner) ].

encode_native_64_signed (X) when is_integer (X) ->
  <<X:64/native-signed>>.

encode_native_64_unsigned (X) when is_integer (X) ->
  <<X:64/native-unsigned>>.

encode_pointer (X) when is_integer (X) ->
  <<X:64/native-unsigned>>.

encode_stat (null) ->
  encode_byte (false);
encode_stat (Stat = #stat{}) ->
  [ encode_byte (true),
    encode_native_64_unsigned (erlang:element (1, Stat#stat.st_dev)),
    encode_native_64_unsigned (erlang:element (2, Stat#stat.st_dev)),
    encode_native_64_unsigned (Stat#stat.st_ino),
    encode_native_64_unsigned (Stat#stat.st_mode),
    encode_native_64_unsigned (Stat#stat.st_nlink),
    encode_native_64_signed (Stat#stat.st_uid),
    encode_native_64_signed (Stat#stat.st_gid),
    encode_native_64_unsigned (erlang:element (1, Stat#stat.st_rdev)),
    encode_native_64_unsigned (erlang:element (2, Stat#stat.st_rdev)),
    encode_native_64_unsigned (Stat#stat.st_size),
    encode_native_64_unsigned (Stat#stat.st_blksize),
    encode_native_64_unsigned (Stat#stat.st_blocks),
    encode_native_64_unsigned (Stat#stat.st_atime),
    encode_native_64_unsigned (Stat#stat.st_mtime),
    encode_native_64_unsigned (Stat#stat.st_ctime) ].

encode_statvfs (StatVFS = #statvfs{}) ->
  [ encode_native_64_unsigned (StatVFS#statvfs.f_bsize),
    encode_native_64_unsigned (StatVFS#statvfs.f_frsize),
    encode_native_64_unsigned (StatVFS#statvfs.f_blocks),
    encode_native_64_unsigned (StatVFS#statvfs.f_bfree),
    encode_native_64_unsigned (StatVFS#statvfs.f_bavail),
    encode_native_64_unsigned (StatVFS#statvfs.f_files),
    encode_native_64_unsigned (StatVFS#statvfs.f_ffree),
    encode_native_64_unsigned (StatVFS#statvfs.f_favail),
    encode_native_64_unsigned (StatVFS#statvfs.f_fsid), 
    encode_native_64_unsigned (StatVFS#statvfs.f_flag), 
    encode_native_64_unsigned (StatVFS#statvfs.f_namemax) ].

encode_string (S) ->
  Len = 1 + erlang:iolist_size (S),
  [ <<Len:64/native-unsigned>>, S, <<0:8>> ].
