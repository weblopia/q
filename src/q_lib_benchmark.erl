%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu
%%% @copyright (C) 2017, <Madalin Grigore-Enescu>
%%% @doc
%%%
%%% @end
%%% Created : 11. Feb 2017 11:35 AM
%%%-------------------------------------------------------------------
-module(q_lib_benchmark).
-author("madalin").

-include_lib("q/include/q.hrl").

%% API
-export([function/4]).

%% @doc Benckmark the result of applying Function in Module to Args.
%% The applied function must be exported from Module.
function(Module, Function, Params, HowManyTimes) ->

  io:format("~nBENCKMARK: Calling ~p:~p/~p function ~p times.", [Module, Function, erlang:length(Params), HowManyTimes]),

  StartTime = q_lib_time:posix_microseconds(),
  apply_function(Module, Function, Params, HowManyTimes),
  EndTime   = q_lib_time:posix_microseconds(),

  %% Time spend
  TimeSpend = EndTime-StartTime,

  %% Display benchmark result
  io:format("~nProcess took ~p seconds", [TimeSpend/1000000]),

  %% Returns time spend
  TimeSpend.

apply_function(_Module, _Function, _Params, 0) -> ok;
apply_function(Module, Function, Params, HowManytimes) ->
  erlang:apply(Module, Function, Params),
  apply_function(Module, Function, Params, HowManytimes-1).
