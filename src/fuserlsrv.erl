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
  when is_atom (Module), is_boolean(LinkedIn),
       is_list (MountOpts), is_list (MountPoint) ->
  gen_server:start (?MODULE, 
                    [ Module, LinkedIn, MountOpts, MountPoint | Args ], 
                    Options).

%% @spec start (ServerName, atom (), bool (), string (), string (), [ any () ], Options) -> Result
%% @doc The analog to gen_server:start/4.  LinkedIn selects either pipe
%% mode (false) or linked-in driver (true).
%% Tip: A useful MountOpts is "debug", which enables debug logging.
%% @end

start (ServerName, Module, LinkedIn, MountOpts, MountPoint, Args, Options) 
  when is_atom (Module), is_boolean(LinkedIn), 
       is_list (MountOpts), is_list (MountPoint) ->
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
  when is_atom (Module), is_boolean(LinkedIn),
       is_list (MountOpts), is_list (MountPoint) ->
  gen_server:start_link (?MODULE, 
                         [ Module, LinkedIn, MountOpts, MountPoint | Args ], 
                         Options).

%% @spec start_link (ServerName, atom (), bool (), string (), string (), [ any () ], Options) -> Result
%% @doc The analog to gen_server:start_link/4.  LinkedIn selects either pipe
%% mode (false) or linked-in driver (true). Tip: A useful MountOpts is "debug",
%% which enables debug logging.
%% @end

start_link (ServerName, Module, LinkedIn, MountOpts, MountPoint, Args, Options) 
  when is_atom (Module), is_boolean(LinkedIn),
       is_list (MountOpts), is_list (MountPoint) ->
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
                      ?uint64(Len), 
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


decode_request (<<?EMULATOR_REQUEST_LL:8,
                  ?uint8(OpCode),
                  ?uint64(Req),
                  ?int64(Uid),
                  ?int64(Gid),
                  ?int64(Pid),
                  ?bin(Rest)>>,
                FusErlSrvState) ->
  Ctx = #fuse_ctx{ uid = Uid, gid = Gid, pid = Pid },
  %% io:format("decode_request ~s\n", [fuserlcodec:decode_opcode(OpCode)]),
  case OpCode of
    ?FUSERL_LL_ACCESS -> access (Req, Ctx, Rest, FusErlSrvState);
    ?FUSERL_LL_CREATE -> create (Req, Ctx, Rest, FusErlSrvState);
    ?FUSERL_LL_FLUSH -> flush (Req, Ctx, Rest, FusErlSrvState);
    ?FUSERL_LL_FORGET -> forget (Req, Ctx, Rest, FusErlSrvState);
    ?FUSERL_LL_FSYNC -> fsync (Req, Ctx, Rest, FusErlSrvState);
    ?FUSERL_LL_FSYNCDIR -> fsyncdir (Req, Ctx, Rest, FusErlSrvState);
    ?FUSERL_LL_GETATTR -> getattr (Req, Ctx, Rest, FusErlSrvState);
    ?FUSERL_LL_GETLK -> getlk (Req, Ctx, Rest, FusErlSrvState);
    ?FUSERL_LL_GETXATTR -> getxattr (Req, Ctx, Rest, FusErlSrvState);
    ?FUSERL_LL_LINK -> link (Req, Ctx, Rest, FusErlSrvState);
    ?FUSERL_LL_LISTXATTR -> listxattr (Req, Ctx, Rest, FusErlSrvState);
    ?FUSERL_LL_LOOKUP -> lookup (Req, Ctx, Rest, FusErlSrvState);
    ?FUSERL_LL_MKDIR -> mkdir (Req, Ctx, Rest, FusErlSrvState);
    ?FUSERL_LL_MKNOD -> mknod (Req, Ctx, Rest, FusErlSrvState);
    ?FUSERL_LL_OPEN -> open (Req, Ctx, Rest, FusErlSrvState);
    ?FUSERL_LL_OPENDIR -> opendir (Req, Ctx, Rest, FusErlSrvState);
    ?FUSERL_LL_READ -> read (Req, Ctx, Rest, FusErlSrvState);
    ?FUSERL_LL_READDIR -> readdir (Req, Ctx, Rest, FusErlSrvState);
    ?FUSERL_LL_READLINK -> readlink (Req, Ctx, Rest, FusErlSrvState);
    ?FUSERL_LL_RELEASE -> release (Req, Ctx, Rest, FusErlSrvState);
    ?FUSERL_LL_RELEASEDIR -> releasedir (Req, Ctx, Rest, FusErlSrvState);
    ?FUSERL_LL_REMOVEXATTR -> removexattr (Req, Ctx, Rest, FusErlSrvState);
    ?FUSERL_LL_RENAME -> rename (Req, Ctx, Rest, FusErlSrvState);
    ?FUSERL_LL_RMDIR -> rmdir (Req, Ctx, Rest, FusErlSrvState);
    ?FUSERL_LL_SETATTR -> setattr (Req, Ctx, Rest, FusErlSrvState);
    ?FUSERL_LL_SETLK -> setlk (Req, Ctx, Rest, FusErlSrvState);
    ?FUSERL_LL_SETXATTR -> setxattr (Req, Ctx, Rest, FusErlSrvState);
    ?FUSERL_LL_STATFS -> statfs (Req, Ctx, Rest, FusErlSrvState);
    ?FUSERL_LL_SYMLINK -> symlink (Req, Ctx, Rest, FusErlSrvState);
    ?FUSERL_LL_UNLINK -> unlink (Req, Ctx, Rest, FusErlSrvState);
    ?FUSERL_LL_WRITE -> write (Req, Ctx, Rest, FusErlSrvState)
  end;
decode_request (X, FusErlSrvState) ->
  % TODO: dispatch to Module handler
  error_logger:error_msg ("fuserlsrv got unexpected data ~p~n", [ X ]),
  { noreply, FusErlSrvState }.

encode_reply (Req, #fuse_reply_err{ err = Err }) ->
    << ?uint8 (?EMULATOR_REPLY_REPLY_LL),
       ?uint64 (Req),
       ?uint8 (?FUSE_REPLY_ERR),
       ?int64(fuserlportable:canonicalize_errno (Err)) >>;
encode_reply (Req, #fuse_reply_none{}) ->
    << ?uint8 (?EMULATOR_REPLY_REPLY_LL),
       ?uint64 (Req),
       ?uint8 (?FUSE_REPLY_NONE) >>;
encode_reply (Req, #fuse_reply_entry{ fuse_entry_param = FuseEntryParam }) ->
    << ?uint8 (?EMULATOR_REPLY_REPLY_LL),
       ?uint64 (Req),
       ?uint8 (?FUSE_REPLY_ENTRY),
       ?bin (fuserlcodec:encode_fuse_entry_param (FuseEntryParam)) >>;
encode_reply (Req, #fuse_reply_create{ fuse_entry_param = FuseEntryParam,
                                       fuse_file_info = Fi }) ->
    << ?uint8 (?EMULATOR_REPLY_REPLY_LL),
       ?uint64 (Req),
       ?uint8 (?FUSE_REPLY_CREATE),
       ?bin(fuserlcodec:encode_fuse_entry_param (FuseEntryParam)),
       ?bin(fuserlcodec:encode_fuse_file_info (Fi)) >>;
encode_reply (Req, #fuse_reply_attr{ attr = Attr, 
                                     attr_timeout_ms = AttrTimeoutMs }) ->
    << ?uint8 (?EMULATOR_REPLY_REPLY_LL),
       ?uint64 (Req),
       ?uint8 (?FUSE_REPLY_ATTR),
       ?bin(fuserlcodec:encode_stat (Attr)),
       ?uint64 (AttrTimeoutMs) >> ;
encode_reply (Req, #fuse_reply_readlink{ link = Link }) ->
    << ?uint8 (?EMULATOR_REPLY_REPLY_LL),
       ?uint64 (Req),
       ?uint8 (?FUSE_REPLY_READLINK),
       ?bin(fuserlcodec:encode_string (Link)) >>;
encode_reply (Req, #fuse_reply_open{ fuse_file_info = Fi }) ->
    << ?uint8 (?EMULATOR_REPLY_REPLY_LL),
       ?uint64 (Req),
       ?uint8 (?FUSE_REPLY_OPEN),
       ?bin( fuserlcodec:encode_fuse_file_info (Fi)) >>;
encode_reply (Req, #fuse_reply_write{ count = Count }) ->
    << ?uint8 (?EMULATOR_REPLY_REPLY_LL),
       ?uint64 (Req),
       ?uint8 (?FUSE_REPLY_WRITE),
       ?uint64 (Count) >>;
encode_reply (Req, #fuse_reply_buf{ buf = Buf, size = Size }) ->
    << ?uint8 (?EMULATOR_REPLY_REPLY_LL),
       ?uint64 (Req),
       ?uint8 (?FUSE_REPLY_BUF),
       ?uint64 (Size),
       ?bin ( iolist_to_binary(Buf) ) >>;
encode_reply (Req, #fuse_reply_statfs{ statvfs = StatVFS }) ->
    << ?uint8 (?EMULATOR_REPLY_REPLY_LL),
       ?uint64 (Req),
       ?uint8 (?FUSE_REPLY_STATFS),
       ?bin ( fuserlcodec:encode_statvfs (StatVFS)) >>;
encode_reply (Req, #fuse_reply_xattr{ count = Count }) ->
    << ?uint8 (?EMULATOR_REPLY_REPLY_LL),
       ?uint64 (Req),
       ?uint8 (?FUSE_REPLY_XATTR),
       ?uint64 (Count) >>;
encode_reply (Req, #fuse_reply_lock{ flock = Flock }) ->
    << ?uint8 (?EMULATOR_REPLY_REPLY_LL),
       ?uint64 (Req),
       ?uint8 (?FUSE_REPLY_LOCK),
       ?bin (fuserlcodec:encode_flock (Flock) )>>;
encode_reply (Req, #fuse_reply_direntrylist{ direntrylist = DirEntryList }) ->
    [ << ?uint8 (?EMULATOR_REPLY_REPLY_LL),
	 ?uint64 (Req),
	 ?uint8 (?FUSE_REPLY_DIRENTRYLIST) >>,
      fuserlcodec:encode_direntry_list (DirEntryList)
    ].

encode_start (Impl, MountOpts, MountPoint) ->
    Bin = iolist_to_binary(Impl),
    Len = byte_size(Bin),
    << ?uint8 (?EMULATOR_REQUEST_START_LL),
       ?bin (fuserlcodec:encode_string (MountOpts)),
       ?bin (fuserlcodec:encode_string (MountPoint)),
       ?uint8 (Len), ?bin (Bin) >>. 

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
    Fs = [ Function ||
	     { Function, Arity } <- fuserl:behaviour_info (callbacks),
	     Function =/= code_change,
	     Function =/= handle_info,
	     Function =/= init,
	     Function =/= terminate,
	     erlang:function_exported (Module, Function, Arity) ],
    io:format(" scan: mod:~p, fs:~p\n", [Module, Fs]),
    [ fuserlcodec:encode_opcode ( F ) || F <- Fs ].


send_reply (Port, Req, Reply) ->
  true = port_command (Port, encode_reply (Req, Reply)).

%-=====================================================================-
%-                             fuse methods                            -
%-=====================================================================-

access (Req, Ctx,
       <<?uint64(Inode),
         ?uint64(Mask)>>,
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
        <<?uint64(Parent),
          ?uint64(Len), 
          ?bin(Name,Len),
          ?uint64(Mode),
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
       <<?uint64(Inode),
         ?bin(Rest)>>,
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
        <<?uint64(Inode),
          ?uint64(Nlookup)>>,
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
       <<?uint64(Inode),
         ?uint8(DataSync),
         ?bin(Rest)>>,
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
          <<?uint64(Inode),
            ?uint8(DataSync),
            ?bin(Rest)>>,
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
         <<?uint64(Inode)>>,
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
       <<?uint64(Inode), ?bin(Rest)>>,
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
          <<?uint64(Inode),
            ?uint64(Len), 
            ?bin(Name,Len),
            ?uint64(Size)>>,
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
      <<?uint64(Ino),
        ?uint64(NewParent),
        ?uint64(NewLen),
        ?bin(NewName,NewLen)>>,
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
           <<?uint64(Ino),
             ?uint64(Size)>>,
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
        <<?uint64(Parent),
          ?uint64(Len),
          ?bin(Name,Len)>>,
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
       <<?uint64(Parent),
         ?uint64(Len),
         ?bin(Name,Len),
         ?uint64(Mode)>>,
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
       <<?uint64(Parent),
         ?uint64(Len),
         ?bin(Name,Len),
         ?uint64(Mode),
         ?uint64(Major),
         ?uint64(Minor)>>,
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
      <<?uint64(Inode),
        ?bin(Rest)>>,
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
         <<?uint64(Inode),
           ?bin(Rest)>>,
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
      <<?uint64(Inode),
        ?uint64(Size),
        ?uint64(Offset),
        ?bin(Rest)>>,
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
         <<?uint64(Inode),
           ?uint64(Size),
           ?uint64(Offset),
           ?bin(Rest)>>,
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
          <<?uint64(Inode)>>,
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
       <<?uint64(Inode),
         ?bin(Rest)>>,
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
            <<?uint64(Inode),
              ?bin(Rest)>>,
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
             <<?uint64(Inode),
               ?uint64(Len),
               ?bin(Name,Len)>>,
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
       <<?uint64(Parent),
         ?uint64(Len),
         ?bin(Name,Len),
         ?uint64(NewParent),
         ?uint64(NewLen),
         ?bin(NewName,NewLen)>>,
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
       <<?uint64(Inode),
         ?uint64(Len),
         ?bin(Name,Len)>>,
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
         <<?uint64(Inode),
           ?int64(ToSet),
           ?bin(Rest)>>,
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
       <<?uint64(Inode), 
         ?uint8(Sleep),
         ?bin(Rest)>>,
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
          <<?uint64(Inode),
            ?uint64(Len),
            ?bin(Name,Len),
            ?uint64(Size),
            ?bin(Value,Size),
            ?int64(Flags)>>,
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
         <<?uint64(Inode)>>,
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
         <<?uint64(LinkLen),
           ?bin(Link,LinkLen),
           ?uint64(Inode),
           ?uint64(Len),
           ?bin(Name,Len)>>,
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
        <<?uint64(Inode),
          ?uint64(Len),
          ?bin(Name,Len)>>,
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
       <<?uint64(Inode),
         ?uint64(Size),
         ?bin(Data,Size),
         ?uint64(Offset),
         ?bin(Rest)>>,
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
