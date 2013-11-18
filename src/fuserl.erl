-module (fuserl).
%-behaviour (fuserl).
-export ([ code_change/3,
           handle_info/2,
           init/1,
           terminate/2,
           access/5,
           create/7,
           flush/5,
           forget/5,
           fsync/6,
           fsyncdir/6,
           getattr/4,
           getlk/6,
           getxattr/6,
           link/6,
           listxattr/5,
           lookup/5,
           mkdir/6,
           mknod/7,
           open/5,
           opendir/5,
           read/7,
           readdir/7,
           readlink/4,
           release/5,
           releasedir/5,
           removexattr/5,
           rename/7,
           rmdir/5,
           setattr/7,
           setlk/7,
           setxattr/7,
           statfs/4,
           symlink/6,
           unlink/5,
           write/7 ]).
%-behaviour (behaviour).
-export ([ behaviour_info/1 ]).

%-=====================================================================-
%-                                Public                               -
%-=====================================================================-

%% @spec behaviour_info (callbacks) -> [ signature () ]
%%   signature () = { init, 1 } |
%%                  { code_change, 3 } |
%%                  { terminate, 2 } |
%%                  { access, 5 } |
%%                  { create, 7 } |
%%                  { flush, 5 } |
%%                  { forget, 5 } |
%%                  { fsync, 6 } |
%%                  { fsyncdir, 6 } |
%%                  { getattr, 4 } |
%%                  { getlk, 6 } |
%%                  { getxattr, 6 } |
%%                  { link, 6 } |
%%                  { listxattr, 5 } |
%%                  { lookup, 5 } |
%%                  { mkdir, 6 } |
%%                  { mknod, 7 } |
%%                  { open, 5 } | 
%%                  { opendir, 5 } | 
%%                  { read, 7 } |
%%                  { readdir, 7 } |
%%                  { readlink, 4 } |
%%                  { release, 5 } |
%%                  { releasedir, 5 } |
%%                  { removexattr, 5 } |
%%                  { rename, 7 } |
%%                  { rmdir, 5 } |
%%                  { setattr, 7 } |
%%                  { setlk, 7 } |
%%                  { setxattr, 7 } |
%%                  { statfs, 4 } |
%%                  { symlink, 6 } |
%%                  { write, 7 } |
%%                  { unlink, 5 }
%% @doc Signature for a FUSE filesystem.  Only init/1, code_change/3, and
%% terminate/2 are required; the rest are optional.
%% @hidden
%% @end

behaviour_info (callbacks) ->
  [ { code_change, 3 },
    { handle_info, 2 },
    { init, 1 },
    { terminate, 2 },
    { access, 5 },
    { create, 7 },
    { flush, 5 },
    { forget, 5 },
    { fsync, 6 },
    { fsyncdir, 6 },
    { getattr, 4 },
    { getlk, 6 },
    { getxattr, 6 },
    { link, 6 },
    { listxattr, 5 },
    { lookup, 5 },
    { mkdir, 6 },
    { mknod, 7 },
    { open, 5 },
    { opendir, 5 },
    { read, 7 },
    { readdir, 7 },
    { readlink, 4 },
    { release, 5 },
    { releasedir, 5 },
    { removexattr, 5 },
    { rename, 7 },
    { rmdir, 5 },
    { setattr, 7 },
    { setlk, 7 },
    { setxattr, 7 },
    { statfs, 4 },
    { symlink, 6 },
    { write, 7 },
    { unlink, 5 } ];
behaviour_info (_Other) ->
  undefined.

%% @spec code_change (OldVsn, State, Extra) -> { ok, NewState }
%%    OldVsn = term () | { down, term () }
%%    Extra = term ()
%% @doc The analog to Module:code_change/3 in gen_server.

code_change (_OldVsn, _State, _Extra) ->
  erlang:throw (not_implemented).

%% @spec handle_info (Msg::any (), State) -> { noreply, NewState } | { noreply, NewState, Timeout } | { stop, Reason, NewState }
%% @doc The analog to Module:handle_info/2 in gen_server.  Note port messages 
%% are intercepted by fuserlsrv and not forwarded to the module.

handle_info (_Msg, _State) ->
  erlang:throw (not_implemented).

%% @spec init (Args) -> { ok, State } | { ok, State, Timeout } | { stop, Reason } | ignore
%% @doc The analog to Module:init/1 in gen_server.
%% @end

init (_Args) ->
  erlang:throw (not_implemented).

%% @spec terminate (Reason, State) -> any ()
%%   Reason = normal | shutdown | term ()
%% @doc The analog to Module:terminate/2 in gen_server.
%% @end

terminate (_Reason, _State) ->
  erlang:throw (not_implemented).

%% @spec access (Ctx::#fuse_ctx{}, Inode::integer (), Mask::access_mode (), Cont::continuation (), State) -> { access_async_reply (), NewState } | { noreply, NewState } 
%%  access_async_reply () = #fuse_reply_err{}
%% @doc Check file access permissions.  Mask is a bitmask consisting of
%% ?F_OK, ?X_OK, ?W_OK, and ?R_OK, which are portably defined in fuserl.hrl .
%% #fuse_reply_err{err = ok} indicates success.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type access_async_reply ().
%% @end

access (_Ctx, _Inode, _Mask, _Cont, _State) ->
  erlang:throw (not_implemented).

%% @spec create (Ctx::#fuse_ctx{}, Parent::integer (), Name::binary (), Mode::create_mode (), Fi::#fuse_file_info{}, Cont::continuation (), State) -> { create_async_reply (), NewState } | { noreply, NewState } 
%%  create_async_reply () = #fuse_reply_create{} | #fuse_reply_err{}
%% @doc Create and open a file.
%% Mode is a bitmask consisting of ?S_XXXXX macros portably defined in 
%% fuserl.hrl .
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type create_async_reply ().
%% @end

create (_Ctx, _Parent, _Name, _Mode, _Fi, _Cont, _State) ->
  erlang:throw (not_implemented).

%% @spec flush (Ctx::#fuse_ctx{}, Inode::integer (), Fi::#fuse_file_info{}, Cont::continuation (), State) -> { flush_async_reply (), NewState } | { noreply, NewState } 
%%  flush_async_reply () = #fuse_reply_err{}
%% @doc This is called on each close () of an opened file, possibly multiple
%% times per {@link open/4. open} call (due to dup () et. al.).  
%% Fi#fuse_file_info.fh will contain the descriptor
%% set in {@link open/4. open}, if any.  
%% #fuse_reply_err{err = ok} indicates success.
%% This return value is ultimately the return value of close ()
%% (unlike {@link release/4. release}).
%% Does *not* necessarily imply an fsync.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type flush_async_reply ().
%% @end

flush (_Ctx, _Inode, _Fi, _Cont, _State) ->
  erlang:throw (not_implemented).

%% @spec forget (Ctx::#fuse_ctx{}, Inode::integer (), Nlookup::integer (), Cont::continuation (), State) -> { forget_async_reply (), NewState } | { noreply, NewState } 
%%  forget_async_reply () = #fuse_reply_none{}
%% @doc Forget about an inode.  If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type forget_async_reply ().
%% @end

forget (_Ctx, _Inode, _Nlookup, _Cont, _State) ->
  erlang:throw (not_implemented).

%% @spec fsync (Ctx::#fuse_ctx{}, Inode::integer (), IsDataSync::bool (), Fi::#fuse_file_info{}, Cont::continuation (), State) -> { fsync_async_reply (), NewState } | { noreply, NewState } 
%%  fsync_async_reply () = #fuse_reply_err{}
%% @doc Ensure all changes are on permanent storage.  If the IsDataSync is
%% true, only the user data should be flushed, not the meta data.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type fsync_async_reply ().
%% @end

fsync (_Ctx, _Inode, _IsDataSync, _Fi, _Cont, _State) ->
  erlang:throw (not_implemented).

%% @spec fsyncdir (Ctx::#fuse_ctx{}, Inode::integer (), IsDataSync::bool (), Fi::#fuse_file_info{}, Cont::continuation (), State) -> { fsyncdir_async_reply (), NewState } | { noreply, NewState } 
%%  fsyncdir_async_reply () = #fuse_reply_err{}
%% @doc Ensure all directory changes are on permanent storage.  
%% If the IsDataSync is
%% true, only the user data should be flushed, not the meta data.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type fsyncdir_async_reply ().
%% @end

fsyncdir (_Ctx, _Inode, _IsDataSync, _Fi, _Cont, _State) ->
  erlang:throw (not_implemented).

%% @spec getattr (Ctx::#fuse_ctx{}, Inode::integer (), Cont::continuation (), State) -> { getattr_async_reply (), NewState } | { noreply, NewState } 
%%  getattr_async_reply () = #fuse_reply_attr{} | #fuse_reply_err{}
%% @doc Get the file attributes associated with an inode.  If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type getattr_async_reply ().
%% @end

getattr (_Ctx, _Inode, _Cont, _State) ->
  erlang:throw (not_implemented).

%% @spec getlk (Ctx::#fuse_ctx{}, Inode::integer (), Fi::#fuse_file_info{}, Lock::#flock{}, Cont::continuation (), State) -> { getlk_async_reply (), NewState } | { noreply, NewState } 
%%  getlk_async_reply () = #fuse_reply_lock{} | #fuse_reply_err{}
%% @doc Test for POSIX file lock.  If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type getlk_async_reply ().
%% @end

getlk (_Ctx, _Inode, _Fi, _Lock, _Cont, _State) ->
  erlang:throw (not_implemented).

%% @spec getxattr (Ctx::#fuse_ctx{}, Inode::integer (), Name::binary (), Size::integer (), Cont::continuation (), State) -> { getxattr_async_reply (), NewState } | { noreply, NewState } 
%%  getxattr_async_reply () = #fuse_reply_buf{} | #fuse_reply_xattr{} | #fuse_reply_err{}
%% @doc Get the value of an extended attribute.  
%% If Size is zero, the size of the value should be sent with
%% #fuse_reply_xattr{}.
%% If Size is non-zero, and the value fits in the buffer, the
%% value should be sent with #fuse_reply_buf{}.
%% If Size is too small for the value, the erange error should
%% be sent.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type getxattr_async_reply ().
%% @end

getxattr (_Ctx, _Inode, _Name, _Size, _Cont, _State) ->
  erlang:throw (not_implemented).

%% @spec link (Ctx::#fuse_ctx{}, Ino::integer (), NewParent::integer (), NewName::binary (), Cont::continuation (), State) -> { link_async_reply (), NewState } | { noreply, NewState }
%%  link_async_reply () = #fuse_reply_entry{} | #fuse_reply_err{}
%% @doc Create a hard link.  Ino is the existing inode.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type link_async_reply ().
%% @end

link (_Ctx, _Ino, _NewParent, _NewName, _Cont, _State) ->
  erlang:throw (not_implemented).

%% @spec listxattr (Ctx::#fuse_ctx{}, Ino::integer (), Size::integer (), Cont::continuation (), State) -> { listxattr_async_reply (), NewState } | { noreply, NewState }
%%  listxattr_async_reply () = #fuse_reply_buf{} | #fuse_reply_xattr{} | #fuse_reply_err{}
%% @doc List extended attribute names.  If Size is zero, the total size 
%% in bytes of the attribute name list (including null terminators)
%% should be sent via #fuse_reply_xattr{}.  
%% If the Size is non-zero, and the null character separated and terminated
%% attribute list is Size or less, the list should be sent with 
%% #fuse_reply_buf{}.
%% If Size is too small for the value, the erange error should
%% be sent.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type listxattr_async_reply ().
%% @end

listxattr (_Ctx, _Ino, _Size, _Cont, _State) ->
  erlang:throw (not_implemented).

%% @spec lookup (Ctx::#fuse_ctx{}, ParentInode::integer (), Name::binary (), Cont::continuation (), State) -> { lookup_async_reply (), NewState } | { noreply, NewState }
%%  lookup_async_reply () = #fuse_reply_entry{} | #fuse_reply_err{}
%% @doc Lookup a directory entry by name and get its attributes.  Returning
%% an entry with inode zero means a negative entry which is cacheable, whereas
%% an error of enoent is a negative entry which is not cacheable.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type lookup_async_reply ().
%% @end

lookup (_Ctx, _ParentInode, _Name, _Cont, _State) ->
  erlang:throw (not_implemented).

%% @spec mkdir (Ctx::#fuse_ctx{}, ParentInode::integer (), Name::binary (), Mode::stat_mode (), Cont::continuation (), State) -> { mkdir_async_reply (), NewState } | { noreply, NewState }
%%  mkdir_async_reply () = #fuse_reply_entry{} | #fuse_reply_err{}
%% @doc Make a directory.  Mode is a mask composed of the ?S_XXXXX macros
%% which are (portably) defined in fuserl.hrl.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type mkdir_async_reply ().
%% @end

mkdir (_Ctx, _ParentInode, _Name, _Mode, _Cont, _State) ->
  erlang:throw (not_implemented).

%% @spec mknod (Ctx::#fuse_ctx{}, ParentInode::integer (), Name::binary (), Mode::stat_mode (), Dev::device (), Cont::continuation (), State) -> { mknod_async_reply (), NewState } | { noreply, NewState }
%%  device () = { Major::integer (), Minor::integer () }
%%  mknod_async_reply () = #fuse_reply_entry{} | #fuse_reply_err{}
%% @doc Create a file node.  Mode is a mask composed of the ?S_XXXXX macros
%% which are (portably) defined in fuserl.hrl.  Dev is only valid if the 
%% created file is a device.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type mknod_async_reply ().
%% @end

mknod (_Ctx, _ParentInode, _Name, _Mode, _Dev, _Cont, _State) ->
  erlang:throw (not_implemented).

%% @spec open (Ctx::#fuse_ctx{}, Inode::integer (), Fi::#fuse_file_info{}, Cont::continuation (), State) -> { open_async_reply (), NewState } | { noreply, NewState }
%%   open_async_reply () = #fuse_reply_open{} | #fuse_reply_err{}
%% @doc Open an inode. If noreply is used, eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} 
%% should be called with Cont as first argument and the second argument of type
%% open_async_reply ().
%% @end

open (_Ctx, _Inode, _Fi, _Cont, _State) ->
  erlang:throw (not_implemented).

%% @spec opendir (Ctx::#fuse_ctx{}, Inode::integer (), Fi::#fuse_file_info{}, Cont::continuation (), State) -> { opendir_async_reply (), NewState } | { noreply, NewState }
%%   opendir_async_reply () = #fuse_reply_open{} | #fuse_reply_err{}
%% @doc Open an directory inode. If noreply is used, eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} 
%% should be called with Cont as first argument and the second argument of type
%% opendir_async_reply ().
%% @end

opendir (_Ctx, _Inode, _Fi, _Cont, _State) ->
  erlang:throw (not_implemented).

%% @spec read (Ctx::#fuse_ctx{}, Inode::integer (), Size::integer (), Offset::integer (), Fi::#fuse_file_info{}, Cont::continuation (), State) -> { read_async_reply (), NewState } | { noreply, NewState }
%%   read_async_reply () = #fuse_reply_buf{} | #fuse_reply_err{}
%% @doc Read Size bytes starting at offset Offset. The file descriptor and
%% other flags are available in Fi. If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type read_async_reply ().
%% @end

read (_Ctx, _Inode, _Size, _Offset, _Fi, _Cont, _State) ->
  erlang:throw (not_implemented).

%% @spec readdir (Ctx::#fuse_ctx{}, Inode::integer (), Size::integer (), Offset::integer (), Fi::#fuse_file_info{}, Cont::continuation (), State) -> { readdir_async_reply (), NewState } | { noreply, NewState }
%%   readdir_async_reply () = #fuse_reply_direntrylist{} | #fuse_reply_err{}
%% @doc Read at most Size bytes at offset Offset from the directory identified
%% Inode.  Size is ``real'' and must be honored: the function 
%% fuserlsrv:dirent_size/1 can be used to compute the aligned byte size of 
%% a direntry, and the size of the list is the sum of the individual sizes.
%% Offsets, however,
%% are ``fake'', and are for the convenience of the implementation to find
%% a specific point in the directory stream.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type readdir_async_reply ().
%% @end

readdir (_Ctx, _Inode, _Size, _Offset, _Fi, _Cont, _State) ->
  erlang:throw (not_implemented).

%% @spec readlink (Ctx::#fuse_ctx{}, Inode::integer (), Cont::continuation (), State) -> { readlink_async_reply (), NewState } | { noreply, NewState }
%%   readlink_async_reply () = #fuse_reply_readlink{} | #fuse_reply_err{}
%% @doc Read the contents of a symbolic link.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type readlink_async_reply ().
%% @end

readlink (_Ctx, _Inode, _Cont, _State) ->
  erlang:throw (not_implemented).

%% @spec release (Ctx::#fuse_ctx{}, Inode::integer (), Fi::#fuse_file_info{}, Cont::continuation (), State) -> { release_async_reply (), NewState } | { noreply, NewState } 
%%  release_async_reply () = #fuse_reply_err{}
%% @doc Called when there are no more references to a file.
%% For every {@link open/4. open} call there is exactly one release call.
%% Fi#fuse_file_info.fh will contain the descriptor
%% set in {@link open/4. open}, if any.  
%% Fi#fuse_file_info.flags will contain the same flags as for 
%% {@link open/4. open}.
%% #fuse_reply_err{err = ok} indicates success.
%% Errors are not reported anywhere; use {@link flush/4. flush} for that.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type release_async_reply ().
%% @end

release (_Ctx, _Inode, _Fi, _Cont, _State) ->
  erlang:throw (not_implemented).

%% @spec releasedir (Ctx::#fuse_ctx{}, Inode::integer (), Fi::#fuse_file_info{}, Cont::continuation (), State) -> { releasedir_async_reply (), NewState } | { noreply, NewState } 
%%  releasedir_async_reply () = #fuse_reply_err{}
%% @doc Called when there are no more references to a directory.
%% For every {@link opendir/4. opendir} call there is exactly one releasedir call.
%% Fi#fuse_file_info.fh will contain the descriptor
%% set in {@link opendir/4. opendir}, if any.  
%% Fi#fuse_file_info.flags will contain the same flags as for 
%% {@link opendir/4. opendir}.
%% #fuse_reply_err{err = ok} indicates success.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type releasedir_async_reply ().
%% @end

releasedir (_Ctx, _Inode, _Fi, _Cont, _State) ->
  erlang:throw (not_implemented).

%% @spec removexattr (Ctx::#fuse_ctx{}, Inode::integer (), Name::binary (), Cont::continuation (), State) -> { removexattr_async_reply (), NewState } | { noreply, NewState }
%%   removexattr_async_reply () = #fuse_reply_err{}
%% @doc Remove an extended attribute.  
%% #fuse_reply_err{err = ok} indicates success.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type removexattr_async_reply ().
%% @end

removexattr (_Ctx, _Inode, _Name, _Cont, _State) ->
  erlang:throw (not_implemented).

%% @spec rename (Ctx::#fuse_ctx{}, Parent::integer (), Name::binary (), NewParent::integer (), NewName::binary (), Cont::continuation (), State) -> { rename_async_reply (), NewState } | { noreply, NewState }
%%   rename_async_reply () = #fuse_reply_err{}
%% @doc Rename a file.  #fuse_reply_err{err = ok} indicates success.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type rename_async_reply ().
%% @end

rename (_Ctx, _Parent, _Name, _NewParent, _NewName, _Cont, _State) ->
  erlang:throw (not_implemented).

%% @spec rmdir (Ctx::#fuse_ctx{}, Inode::integer (), Name::binary (), Cont::continuation (), State) -> { rmdir_async_reply (), NewState } | { noreply, NewState }
%%   rmdir_async_reply () = #fuse_reply_err{}
%% @doc Remove a directory.  #fuse_reply_err{err = ok} indicates success.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type rmdir_async_reply ().
%% @end

rmdir (_Ctx, _Inode, _Name, _Cont, _State) ->
  erlang:throw (not_implemented).

%% @spec setattr (Ctx::#fuse_ctx{}, Inode::integer (), Attr::#stat{}, ToSet::integer (), Fi::maybe_fuse_file_info (), Cont::continuation (), State) -> { setattr_async_reply (), NewState } | { noreply, NewState }
%%   maybe_fuse_file_info () = #fuse_file_info{} | null
%%   setattr_async_reply () = #fuse_reply_attr{} | #fuse_reply_err{}
%% @doc Set file attributes.  ToSet is a bitmask which defines which 
%% elements of Attr are defined and should be modified.  Possible values
%% are defined as ?FUSE_SET_ATTR_XXXX in fuserl.hrl . Fi will be set
%% if setattr is invoked from ftruncate under Linux 2.6.15 or later.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type setattr_async_reply ().
%% @end

setattr (_Ctx, _Inode, _Attr, _ToSet, _Fi, _Cont, _State) ->
  erlang:throw (not_implemented).

%% @spec setlk (Ctx::#fuse_ctx{}, Inode::integer (), Fi::#fuse_file_info{}, Lock::#flock{}, Sleep::bool(), Cont::continuation (), State) -> { setlk_async_reply (), NewState } | { noreply, NewState }
%%   setlk_async_reply () = #fuse_reply_err{}
%% @doc Set a POSIX file lock.  Sleep indicates whether the operation is 
%% blocking (true) or nonblocking (false).
%% #fuse_reply_err{err = ok} indicates success.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type setlk_async_reply ().
%% @end

setlk (_Ctx, _Inode, _Fi, _Lock, _Sleep, _Cont, _State) ->
  erlang:throw (not_implemented).

%% @spec setxattr (Ctx::#fuse_ctx{}, Inode::integer (), Name::binary (), Value::binary (), Flags::xattr_flags (), Cont::continuation (), State) -> { setxattr_async_reply (), NewState } | { noreply, NewState }
%%   setxattr_async_reply () = #fuse_reply_err{}
%% @doc Set file attributes.  
%% #fuse_reply_err{err = ok} indicates success.
%% Flags is a bitmask consisting of ?XATTR_XXXXX macros portably defined in
%% fuserl.hrl .
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called 
%% with Cont as first argument
%% and the second argument of type setxattr_async_reply ().
%% @end

setxattr (_Ctx, _Inode, _Name, _Value, _Flags, _Cont, _State) ->
  erlang:throw (not_implemented).

%% @spec statfs (Ctx::#fuse_ctx{}, Inode::integer (), Cont::continuation (), State) -> { statfs_async_reply (), NewState } | { noreply, NewState }
%%   statfs_async_reply () = #fuse_reply_statfs{} | #fuse_reply_err{}
%% @doc Get file system statistics.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type statfs_async_reply ().
%% @end

statfs (_Ctx, _Inode, _Cont, _State) ->
  erlang:throw (not_implemented).

%% @spec symlink (Ctx::#fuse_ctx{}, Link::binary (), Inode::integer (), Name::binary (), Cont::continuation (), State) -> { symlink_async_reply (), NewState } | { noreply, NewState }
%%   symlink_async_reply () = #fuse_reply_entry{} | #fuse_reply_err{}
%% @doc Create a symbolic link.  Link is the contents of the link.  Name is 
%% the name to create.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type symlink_async_reply ().
%% @end

symlink (_Ctx, _Link, _Inode, _Name, _Cont, _State) ->
  erlang:throw (not_implemented).

%% @spec write (Ctx::#fuse_ctx{}, Inode::integer (), Data::binary (), Offset::integer (), Fi::#fuse_file_info{}, Cont::continuation (), State) -> { write_async_reply (), NewState } | { noreply, NewState }
%%   write_async_reply () = #fuse_reply_write{} | #fuse_reply_err{}
%% @doc Write data to a file.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type write_async_reply ().
%% @end

write (_Ctx, _Inode, _Data, _Offset, _Fi, _Cont, _State) ->
  erlang:throw (not_implemented).

%% @spec unlink (Ctx::#fuse_ctx{}, Inode::integer (), Name::binary (), Cont::continuation (), State) -> { unlink_async_reply (), NewState } | { noreply, NewState }
%%   unlink_async_reply () = #fuse_reply_err{}
%% @doc Remove a file.  #fuse_reply_err{err = ok} indicates success.
%% If noreply is used, 
%% eventually {@link fuserlsrv:reply/2. fuserlsrv:reply/2} should be called with Cont as first argument
%% and the second argument of type unlink_async_reply ().
%% @end

unlink (_Ctx, _Inode, _Name, _Cont, _State) ->
  erlang:throw (not_implemented).
