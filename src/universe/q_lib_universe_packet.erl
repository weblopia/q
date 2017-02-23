%%%-------------------------------------------------------------------
%%% @author madalin
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Feb 2017 6:50 PM
%%%-------------------------------------------------------------------
-module(q_lib_universe_packet).
-author("madalin").

-include_lib("q/include/q.hrl").

%% API
-export([decode/1]).
-export([encode/1]).

decode(Bin) ->