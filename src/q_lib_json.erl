%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu
%%% @copyright (C) 2017, <Madalin Grigore-Enescu>
%%% @doc
%%%
%%% @end
%%% Created : 07. Feb 2017 12:39 AM
%%%-------------------------------------------------------------------
-module(q_lib_json).
-author("madalin").

-include_lib("q/include/q.hrl").

%% API
-export([encode/1, encode/2]).
-export([decode/1, decode/2]).

encode(List) -> throw(qwe_unimplemented).
encode(List, Options) -> throw(qwe_unimplemented).

decode(List) -> throw(qwe_unimplemented).
decode(List, Default) -> throw(qwe_unimplemented).