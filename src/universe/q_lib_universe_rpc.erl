%%%-------------------------------------------------------------------
%%% @author madalin
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Feb 2017 5:51 PM
%%%-------------------------------------------------------------------
-module(q_lib_universe_rpc).
-author("madalin").

%% API
-export([abcast/2, abcast/3]).

abcast(Name, Msg) -> rpc:abcast(Name, Msg).

%% @doc Asynchronous broadcast, returns nothing, it's just send 'n' pray
-spec abcast(Name, Msg) -> abcast when Name :: atom(), Msg :: term().
abcast(Name, Mess) -> abcast([node() | nodes()], Name, Mess).

-spec abcast(Nodes, Name, Msg) -> abcast when Nodes :: [node()], Name :: atom(), Msg :: term().
abcast([Node|Tail], Name, Mess) ->
  Dest = {Name,Node},
  case catch erlang:send(Dest, Mess, [noconnect]) of
    noconnect -> spawn(erlang, send, [Dest,Mess]), ok;
    _ -> ok
  end,
  abcast(Tail, Name, Mess);
abcast([], _,_) -> abcast.

