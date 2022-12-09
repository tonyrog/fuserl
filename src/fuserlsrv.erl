%% @doc An instance of a FUSE filesystem.  Corresponds to version 26 of the
%% FUSE low level API.
%% @end

-module (fuserlsrv).
-export ([ dirent_size/1,
           reply/2,
           start/6,
           start/7,
           start_link/6,
           start_link/7 ]).
-behaviour (gen_server).
-export ([ init/1,
           handle_call/3,
           handle_cast/2,
           handle_info/2,
           terminate/2,
           code_change/3]).

-include ("../include/fuserl.hrl").
%% -include ("fuserlprefix.hrl").

-record (fuserlsrvstate, { module, port, state }).

-define (EMULATOR_REQUEST_LL, 0).
-define (EMULATOR_REPLY_START_LL, 1).

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

-define (EMULATOR_REQUEST_START_LL, 0).
-define (EMULATOR_REPLY_REPLY_LL, 1).

-define (FUSE_REPLY_ERR, 0).
-define (FUSE_REPLY_NONE, 1).
-define (FUSE_REPLY_ENTRY, 2).
-define (FUSE_REPLY_CREATE, 3).
-define (FUSE_REPLY_ATTR, 4).
-define (FUSE_REPLY_READLINK, 5).
-define (FUSE_REPLY_OPEN, 6).
-define (FUSE_REPLY_WRITE, 7).
-define (FUSE_REPLY_BUF, 8).
-define (FUSE_REPLY_IOV, 9).
-define (FUSE_REPLY_STATFS, 10).
-define (FUSE_REPLY_XATTR, 11).
-define (FUSE_REPLY_LOCK, 12).
-define (FUSE_REPLY_BMAP, 13).
-define (FUSE_REPLY_DIRENTRYLIST, 14).

%-=====================================================================-
%-                                Public                               -
%-=====================================================================-

%% @spec dirent_size (#direntry{}) -> integer ()
%% @doc Compute the byte size of a directory entry.
%% @end

dirent_size (#direntry{ name = Name }) ->
  TotalSize = 8 + 8 + 4 + 4 + erlang:iolist_size (Name),
  (TotalSize + 7) band (bnot 7).

%% @spec reply (Cont::continuation (), Info) -> ok
%% @doc Complete an asynchronous operation.  Info is whatever
%% would have been returned by the synchronous operation excepting NewState.
%% @end

reply ({ Port, Req, access }, R=#fuse_reply_err{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, create }, R=#fuse_reply_create{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, create }, R=#fuse_reply_err{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, flush }, R=#fuse_reply_err{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, forget }, R=#fuse_reply_none{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, fsync }, R=#fuse_reply_err{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, fsyncdir }, R=#fuse_reply_err{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, getattr }, R=#fuse_reply_attr{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, getattr }, R=#fuse_reply_err{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, getlk }, R=#fuse_reply_lock{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, getlk }, R=#fuse_reply_err{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, getxattr }, R=#fuse_reply_buf{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, getxattr }, R=#fuse_reply_xattr{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, getxattr }, R=#fuse_reply_err{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, link }, R=#fuse_reply_entry{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, link }, R=#fuse_reply_err{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, listxattr }, R=#fuse_reply_buf{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, listxattr }, R=#fuse_reply_xattr{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, listxattr }, R=#fuse_reply_err{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, lookup }, R=#fuse_reply_entry{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, lookup }, R=#fuse_reply_err{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, mkdir }, R=#fuse_reply_entry{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, mkdir }, R=#fuse_reply_err{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, mknod }, R=#fuse_reply_entry{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, mknod }, R=#fuse_reply_err{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, open }, R=#fuse_reply_open{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, open }, R=#fuse_reply_err{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, opendir }, R=#fuse_reply_open{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, opendir }, R=#fuse_reply_err{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, read }, R=#fuse_reply_buf{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, read }, R=#fuse_reply_err{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, readdir }, R=#fuse_reply_direntrylist{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, readdir }, R=#fuse_reply_err{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, readlink }, R=#fuse_reply_readlink{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, readlink }, R=#fuse_reply_err{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, release }, R=#fuse_reply_err{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, releasedir }, R=#fuse_reply_err{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, removexattr }, R=#fuse_reply_err{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, rename }, R=#fuse_reply_err{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, rmdir }, R=#fuse_reply_err{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, setattr }, R=#fuse_reply_attr{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, setattr }, R=#fuse_reply_err{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, setlk }, R=#fuse_reply_err{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, setxattr }, R=#fuse_reply_err{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, statfs }, R=#fuse_reply_statfs{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, statfs }, R=#fuse_reply_err{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, symlink }, R=#fuse_reply_entry{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, symlink }, R=#fuse_reply_err{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, unlink }, R=#fuse_reply_err{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, write }, R=#fuse_reply_write{}) ->
  send_reply (Port, Req, R);
reply ({ Port, Req, write }, R=#fuse_reply_err{}) ->
  send_reply (Port, Req, R).

%% @spec start (atom (), bool (), string (), string (), [ any () ], Options) -> Result
%% @doc The analog to gen_server:start/3.  LinkedIn selects either pipe 
%% mode (false) or linked-in driver (true).
%% Tip: A useful MountOpts is "debug", which enables debug logging.
%% @end

start (Module, LinkedIn, MountOpts, MountPoint, Args, Options) 
  when is_atom (Module),
       ((LinkedIn =:= true) or (LinkedIn =:= false)),
       is_list (MountOpts),
       is_list (MountPoint) ->
  gen_server:start (?MODULE, 
                    [ Module, LinkedIn, MountOpts, MountPoint | Args ], 
                    Options).

%% @spec start (ServerName, atom (), bool (), string (), string (), [ any () ], Options) -> Result
%% @doc The analog to gen_server:start/4.  LinkedIn selects either pipe
%% mode (false) or linked-in driver (true).
%% Tip: A useful MountOpts is "debug", which enables debug logging.
%% @end

start (ServerName, Module, LinkedIn, MountOpts, MountPoint, Args, Options) 
  when is_atom (Module),
       ((LinkedIn =:= true) or (LinkedIn =:= false)),
       is_list (MountOpts),
       is_list (MountPoint) ->
  gen_server:start (ServerName,
                    ?MODULE, 
                    [ Module, LinkedIn, MountOpts, MountPoint | Args ], 
                    Options).

%% @spec start_link (atom (), bool (), string (), string (), [ any () ], Options) -> Result
%% @doc The analog to gen_server:start_link/3.  LinkedIn selects either pipe
%% mode (false) or linked-in driver (true).  Tip: A useful MountOpts is "debug",
%% which enables debug logging.
%% @end

start_link (Module, LinkedIn, MountOpts, MountPoint, Args, Options) 
  when is_atom (Module),
       ((LinkedIn =:= true) or (LinkedIn =:= false)),
       is_list (MountOpts),
       is_list (MountPoint) ->
  gen_server:start_link (?MODULE, 
                         [ Module, LinkedIn, MountOpts, MountPoint | Args ], 
                         Options).

%% @spec start_link (ServerName, atom (), bool (), string (), string (), [ any () ], Options) -> Result
%% @doc The analog to gen_server:start_link/4.  LinkedIn selects either pipe
%% mode (false) or linked-in driver (true). Tip: A useful MountOpts is "debug",
%% which enables debug logging.
%% @end

start_link (ServerName, Module, LinkedIn, MountOpts, MountPoint, Args, Options) 
  when is_atom (Module),
       ((LinkedIn =:= true) or (LinkedIn =:= false)),
       is_list (MountOpts),
       is_list (MountPoint) ->
  gen_server:start_link (ServerName,
                         ?MODULE, 
                         [ Module, LinkedIn, MountOpts, MountPoint | Args ], 
                         Options).

%-=====================================================================-
%-                         gen_server callbacks                        -
%-=====================================================================-

%% @hidden

init ([ Module, LinkedIn, MountOpts, MountPoint | Args ]) ->
  process_flag (trap_exit, true), 

  Port = make_port (LinkedIn),

  Impl = scan_module (Module),

  true = port_command (Port, encode_start (Impl, MountOpts, MountPoint)),

  receive
    { Port, { data, <<?EMULATOR_REPLY_START_LL:8, 
                      Len:64/native-unsigned, 
                      String:Len/binary>> } } ->
      case String of
        <<"ok">> -> 
          case Module:init (Args) of
            { ok, State } ->
              { ok, #fuserlsrvstate{ module = Module,
                                     port = Port,
                                     state = State } };
            { ok, State, Timeout } ->
              { ok, 
                #fuserlsrvstate{ module = Module,
                                 port = Port,
                                 state = State },
                Timeout };
            R ->
              R
          end;
       <<"error">> ->
          { stop, driver_aborted }
      end
  end.

%% @hidden

handle_call (_Request, _From, State) -> { reply, unimplemented, State }.

%% @hidden

handle_cast (_Request, State) -> { noreply, State }.

%% @hidden

handle_info ({ 'EXIT', Port, Why },
             FusErlSrvState = #fuserlsrvstate{ port = Port }) ->
  %% cheese ... create the fiction that the exit_status message
  %% shows up first
  %% what i really want is priority receive
  receive
    Msg={ Port, { exit_status, _ } } ->
      case handle_info (Msg, FusErlSrvState) of
        { noreply, NewState } ->
          { stop, { port_exit, Why }, NewState };
        { noreply, NewState, _ } ->
          { stop, { port_exit, Why }, NewState };
        R = { stop, _Reason, _NewState } ->
          R
      end
  after 1000 ->
    { stop, { port_exit, Why }, FusErlSrvState }
  end;
handle_info ({ Port, { data, Data } }, 
             FusErlSrvState = #fuserlsrvstate{ port = Port }) ->
  decode_request (Data, FusErlSrvState);
handle_info (Msg, FusErlSrvState = #fuserlsrvstate{ module = Module,
                                                    state = State }) ->
  case Module:handle_info (Msg, State) of
    { noreply, NewState } ->
      { noreply, FusErlSrvState#fuserlsrvstate{ state = NewState } };
    { noreply, NewState, Timeout } ->
      { noreply,
        FusErlSrvState#fuserlsrvstate{ state = NewState },
        Timeout };
    { stop, Reason, NewState } ->
      { stop, Reason, FusErlSrvState#fuserlsrvstate{ state = NewState } }
  end.

%% @hidden

terminate (Reason, #fuserlsrvstate{ module = Module, 
                                    state = State }) ->
  Module:terminate (Reason, State).

% TODO: rescan module functions
%% @hidden

code_change (OldVsn, 
             FusErlSrvState = #fuserlsrvstate{ module = Module, 
                                               state = State },
             Extra) ->
  { ok, NewState } = Module:code_change (OldVsn, State, Extra),

  { ok, FusErlSrvState#fuserlsrvstate{ state = NewState } }.

%-=====================================================================-
%-                               Private                               -
%-=====================================================================-

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

decode_request (<<?EMULATOR_REQUEST_LL:8,
                  OpCode:8,
                  Req:64/native-unsigned,
                  Uid:64/native-signed,
                  Gid:64/native-signed,
                  Pid:64/native-signed,
                  Rest/binary>>,
                FusErlSrvState) ->
  Ctx = #fuse_ctx{ uid = Uid, gid = Gid, pid = Pid },
  case decode_opcode (OpCode) of
    access -> access (Req, Ctx, Rest, FusErlSrvState);
    create -> create (Req, Ctx, Rest, FusErlSrvState);
    flush -> flush (Req, Ctx, Rest, FusErlSrvState);
    forget -> forget (Req, Ctx, Rest, FusErlSrvState);
    fsync -> fsync (Req, Ctx, Rest, FusErlSrvState);
    fsyncdir -> fsyncdir (Req, Ctx, Rest, FusErlSrvState);
    getattr -> getattr (Req, Ctx, Rest, FusErlSrvState);
    getlk -> getlk (Req, Ctx, Rest, FusErlSrvState);
    getxattr -> getxattr (Req, Ctx, Rest, FusErlSrvState);
    link -> link (Req, Ctx, Rest, FusErlSrvState);
    listxattr -> listxattr (Req, Ctx, Rest, FusErlSrvState);
    lookup -> lookup (Req, Ctx, Rest, FusErlSrvState);
    mkdir -> mkdir (Req, Ctx, Rest, FusErlSrvState);
    mknod -> mknod (Req, Ctx, Rest, FusErlSrvState);
    open -> open (Req, Ctx, Rest, FusErlSrvState);
    opendir -> opendir (Req, Ctx, Rest, FusErlSrvState);
    read -> read (Req, Ctx, Rest, FusErlSrvState);
    readdir -> readdir (Req, Ctx, Rest, FusErlSrvState);
    readlink -> readlink (Req, Ctx, Rest, FusErlSrvState);
    release -> release (Req, Ctx, Rest, FusErlSrvState);
    releasedir -> releasedir (Req, Ctx, Rest, FusErlSrvState);
    removexattr -> removexattr (Req, Ctx, Rest, FusErlSrvState);
    rename -> rename (Req, Ctx, Rest, FusErlSrvState);
    rmdir -> rmdir (Req, Ctx, Rest, FusErlSrvState);
    setattr -> setattr (Req, Ctx, Rest, FusErlSrvState);
    setlk -> setlk (Req, Ctx, Rest, FusErlSrvState);
    setxattr -> setxattr (Req, Ctx, Rest, FusErlSrvState);
    statfs -> statfs (Req, Ctx, Rest, FusErlSrvState);
    symlink -> symlink (Req, Ctx, Rest, FusErlSrvState);
    unlink -> unlink (Req, Ctx, Rest, FusErlSrvState);
    write -> write (Req, Ctx, Rest, FusErlSrvState)
  end;
decode_request (X, FusErlSrvState) ->
  % TODO: dispatch to Module handler
  error_logger:error_msg ("fuserlsrv got unexpected data ~p~n", [ X ]),
  { noreply, FusErlSrvState }.

encode_opcode (lookup) -> fuserlcodec:encode_byte (?FUSERL_LL_LOOKUP);
encode_opcode (forget) -> fuserlcodec:encode_byte (?FUSERL_LL_FORGET);
encode_opcode (getattr) -> fuserlcodec:encode_byte (?FUSERL_LL_GETATTR);
encode_opcode (setattr) -> fuserlcodec:encode_byte (?FUSERL_LL_SETATTR);
encode_opcode (readlink) -> fuserlcodec:encode_byte (?FUSERL_LL_READLINK);
encode_opcode (mknod) -> fuserlcodec:encode_byte (?FUSERL_LL_MKNOD);
encode_opcode (mkdir) -> fuserlcodec:encode_byte (?FUSERL_LL_MKDIR);
encode_opcode (unlink) -> fuserlcodec:encode_byte (?FUSERL_LL_UNLINK);
encode_opcode (rmdir) -> fuserlcodec:encode_byte (?FUSERL_LL_RMDIR);
encode_opcode (symlink) -> fuserlcodec:encode_byte (?FUSERL_LL_SYMLINK);
encode_opcode (rename) -> fuserlcodec:encode_byte (?FUSERL_LL_RENAME);
encode_opcode (link) -> fuserlcodec:encode_byte (?FUSERL_LL_LINK);
encode_opcode (open) -> fuserlcodec:encode_byte (?FUSERL_LL_OPEN);
encode_opcode (read) -> fuserlcodec:encode_byte (?FUSERL_LL_READ);
encode_opcode (write) -> fuserlcodec:encode_byte (?FUSERL_LL_WRITE);
encode_opcode (flush) -> fuserlcodec:encode_byte (?FUSERL_LL_FLUSH);
encode_opcode (release) -> fuserlcodec:encode_byte (?FUSERL_LL_RELEASE);
encode_opcode (fsync) -> fuserlcodec:encode_byte (?FUSERL_LL_FSYNC);
encode_opcode (opendir) -> fuserlcodec:encode_byte (?FUSERL_LL_OPENDIR);
encode_opcode (readdir) -> fuserlcodec:encode_byte (?FUSERL_LL_READDIR);
encode_opcode (releasedir) -> fuserlcodec:encode_byte (?FUSERL_LL_RELEASEDIR);
encode_opcode (fsyncdir) -> fuserlcodec:encode_byte (?FUSERL_LL_FSYNCDIR);
encode_opcode (statfs) -> fuserlcodec:encode_byte (?FUSERL_LL_STATFS);
encode_opcode (setxattr) -> fuserlcodec:encode_byte (?FUSERL_LL_SETXATTR);
encode_opcode (getxattr) -> fuserlcodec:encode_byte (?FUSERL_LL_GETXATTR);
encode_opcode (listxattr) -> fuserlcodec:encode_byte (?FUSERL_LL_LISTXATTR);
encode_opcode (removexattr) -> fuserlcodec:encode_byte (?FUSERL_LL_REMOVEXATTR);
encode_opcode (access) -> fuserlcodec:encode_byte (?FUSERL_LL_ACCESS);
encode_opcode (create) -> fuserlcodec:encode_byte (?FUSERL_LL_CREATE);
encode_opcode (getlk) -> fuserlcodec:encode_byte (?FUSERL_LL_GETLK);
encode_opcode (setlk) -> fuserlcodec:encode_byte (?FUSERL_LL_SETLK);
encode_opcode (bmap) -> fuserlcodec:encode_byte (?FUSERL_LL_BMAP).

encode_reply (Req, #fuse_reply_err{ err = Err }) ->
  [ fuserlcodec:encode_byte (?EMULATOR_REPLY_REPLY_LL),
    fuserlcodec:encode_native_64_unsigned (Req),
    fuserlcodec:encode_byte (?FUSE_REPLY_ERR),
    fuserlcodec:encode_native_64_signed 
      (fuserlportable:canonicalize_errno (Err)) ];
encode_reply (Req, #fuse_reply_none{}) ->
  [ fuserlcodec:encode_byte (?EMULATOR_REPLY_REPLY_LL),
    fuserlcodec:encode_native_64_unsigned (Req),
    fuserlcodec:encode_byte (?FUSE_REPLY_NONE) ];
encode_reply (Req, #fuse_reply_entry{ fuse_entry_param = FuseEntryParam }) ->
  [ fuserlcodec:encode_byte (?EMULATOR_REPLY_REPLY_LL),
    fuserlcodec:encode_native_64_unsigned (Req),
    fuserlcodec:encode_byte (?FUSE_REPLY_ENTRY),
    fuserlcodec:encode_fuse_entry_param (FuseEntryParam) ];
encode_reply (Req, #fuse_reply_create{ fuse_entry_param = FuseEntryParam,
                                       fuse_file_info = Fi }) ->
  [ fuserlcodec:encode_byte (?EMULATOR_REPLY_REPLY_LL),
    fuserlcodec:encode_native_64_unsigned (Req),
    fuserlcodec:encode_byte (?FUSE_REPLY_CREATE),
    fuserlcodec:encode_fuse_entry_param (FuseEntryParam),
    fuserlcodec:encode_fuse_file_info (Fi) ];
encode_reply (Req, #fuse_reply_attr{ attr = Attr, 
                                     attr_timeout_ms = AttrTimeoutMs }) ->
  [ fuserlcodec:encode_byte (?EMULATOR_REPLY_REPLY_LL),
    fuserlcodec:encode_native_64_unsigned (Req),
    fuserlcodec:encode_byte (?FUSE_REPLY_ATTR),
    fuserlcodec:encode_stat (Attr),
    fuserlcodec:encode_native_64_unsigned (AttrTimeoutMs) ];
encode_reply (Req, #fuse_reply_readlink{ link = Link }) ->
  [ fuserlcodec:encode_byte (?EMULATOR_REPLY_REPLY_LL),
    fuserlcodec:encode_native_64_unsigned (Req),
    fuserlcodec:encode_byte (?FUSE_REPLY_READLINK),
    fuserlcodec:encode_string (Link) ];
encode_reply (Req, #fuse_reply_open{ fuse_file_info = Fi }) ->
  [ fuserlcodec:encode_byte (?EMULATOR_REPLY_REPLY_LL),
    fuserlcodec:encode_native_64_unsigned (Req),
    fuserlcodec:encode_byte (?FUSE_REPLY_OPEN),
    fuserlcodec:encode_fuse_file_info (Fi) ];
encode_reply (Req, #fuse_reply_write{ count = Count }) ->
  [ fuserlcodec:encode_byte (?EMULATOR_REPLY_REPLY_LL),
    fuserlcodec:encode_native_64_unsigned (Req),
    fuserlcodec:encode_byte (?FUSE_REPLY_WRITE),
    fuserlcodec:encode_native_64_unsigned (Count) ];
encode_reply (Req, #fuse_reply_buf{ buf = Buf, size = Size }) ->
  [ fuserlcodec:encode_byte (?EMULATOR_REPLY_REPLY_LL),
    fuserlcodec:encode_native_64_unsigned (Req),
    fuserlcodec:encode_byte (?FUSE_REPLY_BUF),
    fuserlcodec:encode_binary (Size, Buf) ];
encode_reply (Req, #fuse_reply_statfs{ statvfs = StatVFS }) ->
  [ fuserlcodec:encode_byte (?EMULATOR_REPLY_REPLY_LL),
    fuserlcodec:encode_native_64_unsigned (Req),
    fuserlcodec:encode_byte (?FUSE_REPLY_STATFS),
    fuserlcodec:encode_statvfs (StatVFS) ];
encode_reply (Req, #fuse_reply_xattr{ count = Count }) ->
  [ fuserlcodec:encode_byte (?EMULATOR_REPLY_REPLY_LL),
    fuserlcodec:encode_native_64_unsigned (Req),
    fuserlcodec:encode_byte (?FUSE_REPLY_XATTR),
    fuserlcodec:encode_native_64_unsigned (Count) ];
encode_reply (Req, #fuse_reply_lock{ flock = Flock }) ->
  [ fuserlcodec:encode_byte (?EMULATOR_REPLY_REPLY_LL),
    fuserlcodec:encode_native_64_unsigned (Req),
    fuserlcodec:encode_byte (?FUSE_REPLY_LOCK),
    fuserlcodec:encode_flock (Flock) ];
encode_reply (Req, #fuse_reply_direntrylist{ direntrylist = DirEntryList }) ->
  [ fuserlcodec:encode_byte (?EMULATOR_REPLY_REPLY_LL),
    fuserlcodec:encode_native_64_unsigned (Req),
    fuserlcodec:encode_byte (?FUSE_REPLY_DIRENTRYLIST),
    fuserlcodec:encode_direntry_list (DirEntryList) ].

encode_start (Impl, MountOpts, MountPoint) ->
  [ fuserlcodec:encode_byte (?EMULATOR_REQUEST_START_LL),
    fuserlcodec:encode_string (MountOpts),
    fuserlcodec:encode_string (MountPoint),
    fuserlcodec:encode_byte (erlang:length (Impl)),
    Impl ].

%% fuserldrv_guessprefix () ->
%%   lists:foldl (fun (Candidate, undefined) ->
%%                  case file:read_file_info (Candidate ++ "/bin/fuserldrv") of
%%                    { ok, _ } -> Candidate;
%%                    { error, _ } -> undefined
%%                  end;
%%                    (_, Acc) ->
%%                  Acc
%%                end,
%%                undefined,
%%                [ ?automake_prefix, "/usr", "/usr/local", "/sw" ]).

make_port (false) ->
    Dir = code:priv_dir(fuserl),
%%  { ok, DirVar } = application:get_env (fuserl, fuserldrvprefix),
%%
%%  Dir0 = case DirVar of auto_detect -> fuserldrv_guessprefix (); _ -> DirVar end,
%%  Dir = filenamejoin(Dir, "bin"),
%%
%  open_port ({ spawn, "/usr/bin/valgrind --log-file=/tmp/flass --num-callers=50 --tool=memcheck --track-fds=yes --leak-check=yes --show-reachable=yes " ++ Dir ++ "/bin/fuserldrv" }, [ binary, 

%  open_port ({ spawn, "ktrace -id -tcu " ++ Dir ++ "/bin/fuserldrv" }, [ binary, 

%  open_port ({ spawn, "strace -f -ff -e trace=file,desc -o flass " ++ Dir ++ "/bin/fuserldrv" }, [ binary,

  open_port ({ spawn, filename:join(Dir,"fuserldrv") },
	     [ binary, { packet, 4 },nouse_stdio, exit_status ]);
make_port (true) ->
    Dir = code:priv_dir(fuserl),
%%  { ok, DirVar } = application:get_env (fuserl, fuserldrvprefix),
%%
%%  Dir0 = case DirVar of auto_detect -> fuserldrv_guessprefix (); _ -> DirVar end,
%%  Dir  = filename:hoin(Dir,"lib")
  case erl_ddll:load_driver (Dir, fuserl) of
    ok -> ok;
    { error, already_loaded } -> ok
  end,

  open_port ({ spawn, fuserl }, [ binary ]).

scan_module (Module) ->
  { module, Module } = code:ensure_loaded (Module),

  [ encode_opcode (Function) ||
    { Function, Arity } <- fuserl:behaviour_info (callbacks),
    Function =/= code_change,
    Function =/= handle_info,
    Function =/= init,
    Function =/= terminate,
    erlang:function_exported (Module, Function, Arity) ].

send_reply (Port, Req, Reply) ->
  true = port_command (Port, encode_reply (Req, Reply)).

%-=====================================================================-
%-                             fuse methods                            -
%-=====================================================================-

access (Req, Ctx,
       <<Inode:64/native-unsigned,
         Mask:64/native-unsigned>>,
       FusErlSrvState = #fuserlsrvstate{ module = Module, 
                                         port = Port,
                                         state = State }) ->
  Cont = { Port, Req, access },
  case Module:access (Ctx, Inode, Mask, Cont, State) of
    { noreply, NewState } ->
      ok;
    { R=#fuse_reply_err{}, NewState } ->
      send_reply (Port, Req, R)
  end,

  { noreply, FusErlSrvState#fuserlsrvstate{ state = NewState } }.

create (Req, Ctx,
        <<Parent:64/native-unsigned,
          Len:64/native-unsigned, 
          Name:Len/binary,
          Mode:64/native-unsigned,
          Rest/binary>>,
        FusErlSrvState = #fuserlsrvstate{ module = Module, 
                                          port = Port,
                                          state = State }) ->
  { Fi, _ } = fuserlcodec:decode_ffi (Rest),
  Cont = { Port, Req, create },
  case Module:create (Ctx, Parent, Name, Mode, Fi, Cont, State) of
    { noreply, NewState } ->
      ok;
    { R=#fuse_reply_create{}, NewState } ->
      send_reply (Port, Req, R);
    { R=#fuse_reply_err{}, NewState } ->
      send_reply (Port, Req, R)
  end,

  { noreply, FusErlSrvState#fuserlsrvstate{ state = NewState } }.

flush (Req, Ctx,
       <<Inode:64/native-unsigned,
         Rest/binary>>,
       FusErlSrvState = #fuserlsrvstate{ module = Module, 
                                         port = Port,
                                         state = State }) ->
  { Fi, _ } = fuserlcodec:decode_ffi (Rest),
  Cont = { Port, Req, flush },
  case Module:flush (Ctx, Inode, Fi, Cont, State) of
    { noreply, NewState } ->
      ok;
    { R=#fuse_reply_err{}, NewState } ->
      send_reply (Port, Req, R)
  end,

  { noreply, FusErlSrvState#fuserlsrvstate{ state = NewState } }.

forget (Req, Ctx,
        <<Inode:64/native-unsigned,
          Nlookup:64/native-unsigned>>,
        FusErlSrvState = #fuserlsrvstate{ module = Module, 
                                          port = Port,
                                          state = State }) ->
  Cont = { Port, Req, forget },
  case Module:forget (Ctx, Inode, Nlookup, Cont, State) of
    { noreply, NewState } ->
      ok;
    { R=#fuse_reply_none{}, NewState } ->
      send_reply (Port, Req, R)
  end,

  { noreply, FusErlSrvState#fuserlsrvstate{ state = NewState } }.

fsync (Req, Ctx,
       <<Inode:64/native-unsigned,
         DataSync:8,
         Rest/binary>>,
       FusErlSrvState = #fuserlsrvstate{ module = Module, 
                                         port = Port,
                                         state = State }) ->
  { Fi, _ } = fuserlcodec:decode_ffi (Rest),
  Cont = { Port, Req, fsync },
  case Module:fsync (Ctx, Inode, (DataSync =/= 0), Fi, Cont, State) of
    { noreply, NewState } ->
      ok;
    { R=#fuse_reply_err{}, NewState } ->
      send_reply (Port, Req, R)
  end,

  { noreply, FusErlSrvState#fuserlsrvstate{ state = NewState } }.

fsyncdir (Req, Ctx,
          <<Inode:64/native-unsigned,
            DataSync:8,
            Rest/binary>>,
          FusErlSrvState = #fuserlsrvstate{ module = Module, 
                                            port = Port,
                                            state = State }) ->
  { Fi, _ } = fuserlcodec:decode_ffi (Rest),
  Cont = { Port, Req, fsyncdir },
  case Module:fsyncdir (Ctx, Inode, (DataSync =/= 0), Fi, Cont, State) of
    { noreply, NewState } ->
      ok;
    { R=#fuse_reply_err{}, NewState } ->
      send_reply (Port, Req, R)
  end,

  { noreply, FusErlSrvState#fuserlsrvstate{ state = NewState } }.

getattr (Req, Ctx,
         <<Inode:64/native-unsigned>>,
         FusErlSrvState = #fuserlsrvstate{ module = Module, 
                                           port = Port,
                                           state = State }) ->
  Cont = { Port, Req, getattr },
  case Module:getattr (Ctx, Inode, Cont, State) of
    { noreply, NewState } ->
      ok;
    { R=#fuse_reply_attr{}, NewState } ->
      send_reply (Port, Req, R);
    { R=#fuse_reply_err{}, NewState } ->
      send_reply (Port, Req, R)
  end,

  { noreply, FusErlSrvState#fuserlsrvstate{ state = NewState } }.

getlk (Req, Ctx,
       <<Inode:64/native-unsigned, Rest/binary>>,
       FusErlSrvState = #fuserlsrvstate{ module = Module, 
                                         port = Port,
                                         state = State }) ->
  { Fi, Rest2 } = fuserlcodec:decode_ffi (Rest),
  { Lock, _ } = fuserlcodec:decode_flock (Rest2),
  Cont = { Port, Req, getlk },
  case Module:getlk (Ctx, Inode, Fi, Lock, Cont, State) of
    { noreply, NewState } ->
      ok;
    { R=#fuse_reply_lock{}, NewState } ->
      send_reply (Port, Req, R);
    { R=#fuse_reply_err{}, NewState } ->
      send_reply (Port, Req, R)
  end,

  { noreply, FusErlSrvState#fuserlsrvstate{ state = NewState } }.

getxattr (Req, Ctx,
          <<Inode:64/native-unsigned,
            Len:64/native-unsigned, 
            Name:Len/binary,
            Size:64/native-unsigned>>,
          FusErlSrvState = #fuserlsrvstate{ module = Module, 
                                            port = Port,
                                            state = State }) ->
  Cont = { Port, Req, getxattr },
  case Module:getxattr (Ctx, Inode, Name, Size, Cont, State) of
    { noreply, NewState } ->
      ok;
    { R=#fuse_reply_buf{}, NewState } ->
      send_reply (Port, Req, R);
    { R=#fuse_reply_xattr{}, NewState } ->
      send_reply (Port, Req, R);
    { R=#fuse_reply_err{}, NewState } ->
      send_reply (Port, Req, R)
  end,

  { noreply, FusErlSrvState#fuserlsrvstate{ state = NewState } }.

link (Req, Ctx, 
      <<Ino:64/native-unsigned,
        NewParent:64/native-unsigned,
        NewLen:64/native-unsigned,
        NewName:NewLen/binary>>,
      FusErlSrvState = #fuserlsrvstate{ module = Module, 
                                        port = Port,
                                        state = State }) ->
  Cont = { Port, Req, link },
  case Module:link (Ctx, Ino, NewParent, NewName, Cont, State) of
    { noreply, NewState } ->
      ok;
    { R=#fuse_reply_entry{}, NewState } ->
      send_reply (Port, Req, R);
    { R=#fuse_reply_err{}, NewState } ->
      send_reply (Port, Req, R)
  end,

  { noreply, FusErlSrvState#fuserlsrvstate{ state = NewState } }.

listxattr (Req, Ctx, 
           <<Ino:64/native-unsigned,
             Size:64/native-unsigned>>,
           FusErlSrvState = #fuserlsrvstate{ module = Module, 
                                             port = Port,
                                             state = State }) ->
  Cont = { Port, Req, listxattr },
  case Module:listxattr (Ctx, Ino, Size, Cont, State) of
    { noreply, NewState } ->
      ok;
    { R=#fuse_reply_buf{}, NewState } ->
      send_reply (Port, Req, R);
    { R=#fuse_reply_xattr{}, NewState } ->
      send_reply (Port, Req, R);
    { R=#fuse_reply_err{}, NewState } ->
      send_reply (Port, Req, R)
  end,

  { noreply, FusErlSrvState#fuserlsrvstate{ state = NewState } }.

lookup (Req, Ctx, 
        <<Parent:64/native-unsigned,
          Len:64/native-unsigned,
          Name:Len/binary>>,
        FusErlSrvState = #fuserlsrvstate{ module = Module, 
                                          port = Port,
                                          state = State }) ->
  Cont = { Port, Req, lookup },
  case Module:lookup (Ctx, Parent, Name, Cont, State) of
    { noreply, NewState } ->
      ok;
    { R=#fuse_reply_entry{}, NewState } ->
      send_reply (Port, Req, R);
    { R=#fuse_reply_err{}, NewState } ->
      send_reply (Port, Req, R)
  end,

  { noreply, FusErlSrvState#fuserlsrvstate{ state = NewState } }.

mkdir (Req, Ctx, 
       <<Parent:64/native-unsigned,
         Len:64/native-unsigned,
         Name:Len/binary,
         Mode:64/native-unsigned>>,
       FusErlSrvState = #fuserlsrvstate{ module = Module, 
                                         port = Port,
                                         state = State }) ->
  Cont = { Port, Req, mkdir },
  case Module:mkdir (Ctx, Parent, Name, Mode, Cont, State) of
    { noreply, NewState } ->
      ok;
    { R=#fuse_reply_entry{}, NewState } ->
      send_reply (Port, Req, R);
    { R=#fuse_reply_err{}, NewState } ->
      send_reply (Port, Req, R)
  end,

  { noreply, FusErlSrvState#fuserlsrvstate{ state = NewState } }.

mknod (Req, Ctx, 
       <<Parent:64/native-unsigned,
         Len:64/native-unsigned,
         Name:Len/binary,
         Mode:64/native-unsigned,
         Major:64/native-unsigned,
         Minor:64/native-unsigned>>,
       FusErlSrvState = #fuserlsrvstate{ module = Module, 
                                         port = Port,
                                         state = State }) ->
  Cont = { Port, Req, mknod },
  case Module:mknod (Ctx, Parent, Name, Mode, { Major, Minor }, Cont, State) of
    { noreply, NewState } ->
      ok;
    { R=#fuse_reply_entry{}, NewState } ->
      send_reply (Port, Req, R);
    { R=#fuse_reply_err{}, NewState } ->
      send_reply (Port, Req, R)
  end,

  { noreply, FusErlSrvState#fuserlsrvstate{ state = NewState } }.

open (Req, Ctx, 
      <<Inode:64/native-unsigned,
        Rest/binary>>,
      FusErlSrvState = #fuserlsrvstate{ module = Module, 
                                        port = Port,
                                        state = State }) ->
  { Fi, _ } = fuserlcodec:decode_ffi (Rest),
  Cont = { Port, Req, open },
  case Module:open (Ctx, Inode, Fi, Cont, State) of 
    { noreply, NewState } ->
      ok;
    { R=#fuse_reply_open{}, NewState } ->
      send_reply (Port, Req, R);
    { R=#fuse_reply_err{}, NewState } ->
      send_reply (Port, Req, R)
  end,

  { noreply, FusErlSrvState#fuserlsrvstate{ state = NewState } }.

opendir (Req, Ctx, 
         <<Inode:64/native-unsigned,
           Rest/binary>>,
         FusErlSrvState = #fuserlsrvstate{ module = Module, 
                                           port = Port,
                                           state = State }) ->
  { Fi, _ } = fuserlcodec:decode_ffi (Rest),
  Cont = { Port, Req, opendir },
  case Module:opendir (Ctx, Inode, Fi, Cont, State) of 
    { noreply, NewState } ->
      ok;
    { R=#fuse_reply_open{}, NewState } ->
      send_reply (Port, Req, R);
    { R=#fuse_reply_err{}, NewState } ->
      send_reply (Port, Req, R)
  end,

  { noreply, FusErlSrvState#fuserlsrvstate{ state = NewState } }.

read (Req, Ctx, 
      <<Inode:64/native-unsigned,
        Size:64/native-unsigned,
        Offset:64/native-unsigned,
        Rest/binary>>,
      FusErlSrvState = #fuserlsrvstate{ module = Module, 
                                        port = Port,
                                        state = State }) ->
  { Fi, _ } = fuserlcodec:decode_ffi (Rest),
  Cont = { Port, Req, read },
  case Module:read (Ctx, Inode, Size, Offset, Fi, Cont, State) of
    { noreply, NewState } ->
      ok;
    { R=#fuse_reply_buf{}, NewState } ->
      send_reply (Port, Req, R);
    { R=#fuse_reply_err{}, NewState } ->
      send_reply (Port, Req, R)
  end,

  { noreply, FusErlSrvState#fuserlsrvstate{ state = NewState } }.

readdir (Req, Ctx, 
         <<Inode:64/native-unsigned,
           Size:64/native-unsigned,
           Offset:64/native-unsigned,
           Rest/binary>>,
         FusErlSrvState = #fuserlsrvstate{ module = Module, 
                                           port = Port,
                                           state = State }) ->
  { Fi, _ } = fuserlcodec:decode_ffi (Rest),
  Cont = { Port, Req, readdir },
  case Module:readdir (Ctx, Inode, Size, Offset, Fi, Cont, State) of
    { noreply, NewState } ->
      ok;
    { R=#fuse_reply_direntrylist{}, NewState } ->
      send_reply (Port, Req, R);
    { R=#fuse_reply_err{}, NewState } ->
      send_reply (Port, Req, R)
  end,

  { noreply, FusErlSrvState#fuserlsrvstate{ state = NewState } }.

readlink (Req, Ctx, 
          <<Inode:64/native-unsigned>>,
          FusErlSrvState = #fuserlsrvstate{ module = Module, 
                                            port = Port,
                                            state = State }) ->
  Cont = { Port, Req, readlink },
  case Module:readlink (Ctx, Inode, Cont, State) of
    { noreply, NewState } ->
      ok;
    { R=#fuse_reply_readlink{}, NewState } ->
      send_reply (Port, Req, R);
    { R=#fuse_reply_err{}, NewState } ->
      send_reply (Port, Req, R)
  end,

  { noreply, FusErlSrvState#fuserlsrvstate{ state = NewState } }.

release (Req, Ctx,
       <<Inode:64/native-unsigned,
         Rest/binary>>,
       FusErlSrvState = #fuserlsrvstate{ module = Module, 
                                         port = Port,
                                         state = State }) ->
  { Fi, _ } = fuserlcodec:decode_ffi (Rest),
  Cont = { Port, Req, release },
  case Module:release (Ctx, Inode, Fi, Cont, State) of
    { noreply, NewState } ->
      ok;
    { R=#fuse_reply_err{}, NewState } ->
      send_reply (Port, Req, R)
  end,

  { noreply, FusErlSrvState#fuserlsrvstate{ state = NewState } }.

releasedir (Req, Ctx,
            <<Inode:64/native-unsigned,
              Rest/binary>>,
            FusErlSrvState = #fuserlsrvstate{ module = Module, 
                                              port = Port,
                                              state = State }) ->
  { Fi, _ } = fuserlcodec:decode_ffi (Rest),
  Cont = { Port, Req, releasedir },
  case Module:releasedir (Ctx, Inode, Fi, Cont, State) of
    { noreply, NewState } ->
      ok;
    { R=#fuse_reply_err{}, NewState } ->
      send_reply (Port, Req, R)
  end,

  { noreply, FusErlSrvState#fuserlsrvstate{ state = NewState } }.

removexattr (Req, Ctx, 
             <<Inode:64/native-unsigned,
               Len:64/native-unsigned,
               Name:Len/binary>>,
             FusErlSrvState = #fuserlsrvstate{ module = Module, 
                                               port = Port,
                                               state = State }) ->
  Cont = { Port, Req, removexattr },
  case Module:removexattr (Ctx, Inode, Name, Cont, State) of
    { noreply, NewState } ->
      ok;
    { R=#fuse_reply_err{}, NewState } ->
      send_reply (Port, Req, R)
  end,

  { noreply, FusErlSrvState#fuserlsrvstate{ state = NewState } }.

rename (Req, Ctx, 
       <<Parent:64/native-unsigned,
         Len:64/native-unsigned,
         Name:Len/binary,
         NewParent:64/native-unsigned,
         NewLen:64/native-unsigned,
         NewName:NewLen/binary>>,
       FusErlSrvState = #fuserlsrvstate{ module = Module, 
                                         port = Port,
                                         state = State }) ->
  Cont = { Port, Req, rename },
  case Module:rename (Ctx, Parent, Name, NewParent, NewName, Cont, State) of
    { noreply, NewState } ->
      ok;
    { R=#fuse_reply_err{}, NewState } ->
      send_reply (Port, Req, R)
  end,

  { noreply, FusErlSrvState#fuserlsrvstate{ state = NewState } }.

rmdir (Req, Ctx, 
       <<Inode:64/native-unsigned,
         Len:64/native-unsigned,
         Name:Len/binary>>,
       FusErlSrvState = #fuserlsrvstate{ module = Module, 
                                         port = Port,
                                         state = State }) ->
  Cont = { Port, Req, rmdir },
  case Module:rmdir (Ctx, Inode, Name, Cont, State) of
    { noreply, NewState } ->
      ok;
    { R=#fuse_reply_err{}, NewState } ->
      send_reply (Port, Req, R)
  end,

  { noreply, FusErlSrvState#fuserlsrvstate{ state = NewState } }.

setattr (Req, Ctx, 
         <<Inode:64/native-unsigned,
           ToSet:64/native-signed,
           Rest/binary>>,
         FusErlSrvState = #fuserlsrvstate{ module = Module, 
                                           port = Port,
                                           state = State }) ->
  { Attr, Rest2 } = fuserlcodec:decode_stat (Rest),
  { Fi, _ } = fuserlcodec:decode_ffi_ptr (Rest2),
  Cont = { Port, Req, setattr },
  case Module:setattr (Ctx, Inode, Attr, ToSet, Fi, Cont, State) of
    { noreply, NewState } ->
      ok;
    { R=#fuse_reply_attr{}, NewState } ->
      send_reply (Port, Req, R);
    { R=#fuse_reply_err{}, NewState } ->
      send_reply (Port, Req, R)
  end,

  { noreply, FusErlSrvState#fuserlsrvstate{ state = NewState } }.

setlk (Req, Ctx, 
       <<Inode:64/native-unsigned, 
         Sleep:8,
         Rest/binary>>,
       FusErlSrvState = #fuserlsrvstate{ module = Module, 
                                         port = Port,
                                         state = State }) ->
  { Fi, Rest2 } = fuserlcodec:decode_ffi (Rest),
  { Lock, _ } = fuserlcodec:decode_flock (Rest2),
  Cont = { Port, Req, setlk },
  case Module:setlk (Ctx, Inode, Fi, Lock, Sleep =/= 0, Cont, State) of
    { noreply, NewState } ->
      ok;
    { R=#fuse_reply_err{}, NewState } ->
      send_reply (Port, Req, R)
  end,

  { noreply, FusErlSrvState#fuserlsrvstate{ state = NewState } }.

setxattr (Req, Ctx, 
          <<Inode:64/native-unsigned,
            Len:64/native-unsigned,
            Name:Len/binary,
            Size:64/native-unsigned,
            Value:Size/binary,
            Flags:64/native-signed>>,
          FusErlSrvState = #fuserlsrvstate{ module = Module, 
                                            port = Port,
                                            state = State }) ->
  Cont = { Port, Req, setxattr },
  case Module:setxattr (Ctx, Inode, Name, Value, Flags, Cont, State) of
    { noreply, NewState } ->
      ok;
    { R=#fuse_reply_err{}, NewState } ->
      send_reply (Port, Req, R)
  end,

  { noreply, FusErlSrvState#fuserlsrvstate{ state = NewState } }.

statfs (Req, Ctx, 
         <<Inode:64/native-unsigned>>,
         FusErlSrvState = #fuserlsrvstate{ module = Module, 
                                           port = Port,
                                           state = State }) ->
  Cont = { Port, Req, statfs },
  case Module:statfs (Ctx, Inode, Cont, State) of
    { noreply, NewState } ->
      ok;
    { R=#fuse_reply_statfs{}, NewState } ->
      send_reply (Port, Req, R);
    { R=#fuse_reply_err{}, NewState } ->
      send_reply (Port, Req, R)
  end,

  { noreply, FusErlSrvState#fuserlsrvstate{ state = NewState } }.

symlink (Req, Ctx, 
         <<LinkLen:64/native-unsigned,
           Link:LinkLen/binary,
           Inode:64/native-unsigned,
           Len:64/native-unsigned,
           Name:Len/binary>>,
         FusErlSrvState = #fuserlsrvstate{ module = Module, 
                                           port = Port,
                                           state = State }) ->
  Cont = { Port, Req, symlink },
  case Module:symlink (Ctx, Link, Inode, Name, Cont, State) of
    { noreply, NewState } ->
      ok;
    { R=#fuse_reply_entry{}, NewState } ->
      send_reply (Port, Req, R);
    { R=#fuse_reply_err{}, NewState } ->
      send_reply (Port, Req, R)
  end,

  { noreply, FusErlSrvState#fuserlsrvstate{ state = NewState } }.

unlink (Req, Ctx, 
        <<Inode:64/native-unsigned,
          Len:64/native-unsigned,
          Name:Len/binary>>,
        FusErlSrvState = #fuserlsrvstate{ module = Module, 
                                          port = Port,
                                          state = State }) ->
  Cont = { Port, Req, unlink },
  case Module:unlink (Ctx, Inode, Name, Cont, State) of
    { noreply, NewState } ->
      ok;
    { R=#fuse_reply_err{}, NewState } ->
      send_reply (Port, Req, R)
  end,

  { noreply, FusErlSrvState#fuserlsrvstate{ state = NewState } }.

write (Req, Ctx, 
       <<Inode:64/native-unsigned,
         Size:64/native-unsigned,
         Data:Size/binary,
         Offset:64/native-unsigned,
         Rest/binary>>,
       FusErlSrvState = #fuserlsrvstate{ module = Module, 
                                         port = Port,
                                         state = State }) ->
  { Fi, _ } = fuserlcodec:decode_ffi (Rest),
  Cont = { Port, Req, write },
  case Module:write (Ctx, Inode, Data, Offset, Fi, Cont, State) of
    { noreply, NewState } ->
      ok;
    { R=#fuse_reply_write{}, NewState } ->
      send_reply (Port, Req, R);
    { R=#fuse_reply_err{}, NewState } ->
      send_reply (Port, Req, R)
  end,

  { noreply, FusErlSrvState#fuserlsrvstate{ state = NewState } }.
