%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2023, Tony Rogvall
%%% @doc
%%%    NIF version of fuserl
%%% @end
%%% Created : 16 Jan 2023 by Tony Rogvall <tony@rogvall.se>

-module(fuserl_nif).

-on_load(init/0).
-export([mount/3]).
-export([unmount/1]).
-export([process/1]).
-export([reply/3]).

-define(nif_stub(),
	erlang:nif_error({nif_not_loaded,module,?MODULE,line,?LINE})).

init() ->
    Nif = filename:join(code:priv_dir(fuserl), "fuserl_nif"),
    erlang:load_nif(Nif, 0).

-spec mount(MountOpts::string(), MountPoint::string(), OpList::[atom()]) ->
	  Handle::reference().
mount(_MountOpts, _MoutPoint, _OpList) ->
    ?nif_stub().

-spec unmount(Handle::reference()) -> ok | {error,Reason::term()}.
unmount(_Handle) ->
    ?nif_stub().

-spec process(Handle::reference()) -> ok | select | {error,Reason::term()}.
process(_Handle) ->
    ?nif_stub().

-spec reply(Handle::reference(), Req::integer(), Reply::term()) -> ok.
reply(_Handle, _Req, _Reply) ->
    ?nif_stub().
