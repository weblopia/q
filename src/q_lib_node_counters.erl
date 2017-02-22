%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu
%%% @copyright (C) 2017, <Madalin Grigore-Enescu>
%%% @doc
%%%
%%% @end
%%% Created : 08. Feb 2017 9:43 PM
%%%-------------------------------------------------------------------
-module(q_lib_node_counters).
-author("madalin").

-include_lib("q/include/q.hrl").

%% API
-export([increase/1]).
-export([increase/2]).
-export([get/1]).
-export([set/2]).
-export([reset/1]).

-export([max_length_increase/1]).
-export([max_length_increase/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% basic operations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Increase the specified node counter by 1
increase(CounterName) -> increase(CounterName, 1).

%% @doc Increase the specified counter by the specified value
increase(CounterName, Value) when erlang:is_integer(Value) ->
    ets:update_counter(?Q_ETS_TABLE_COUNTERS, CounterName, Value, Value).

%% @doc Returns the specified counter value
get(CounterName) ->
  [Value] = ets:lookup(?Q_ETS_TABLE_COUNTERS, CounterName),
  Value.

%% @doc Sets the specified counter value
set(CounterName, Value) when erlang:is_integer(Value) ->
  ets:insert(?Q_ETS_TABLE_COUNTERS, {CounterName, Value}).

%% @doc Reset the specified counter to 0
reset(CounterName) -> ets:insert(?Q_ETS_TABLE_COUNTERS, {CounterName, 0}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% max_length counters operations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Increase the maximum length counter by one or reset the counter to 0 if the counter maximum bytes length is exceded.
%% Counter length is the number of alphabetic bytes neccessary to display the counter integer value.
%% Counter name is <<"qwemaxlen", erlang:integer_to_binary(Length)/binary>>
max_length_increase(Length) when erlang:is_integer(Length) ->

  BinLength   = erlang:integer_to_binary(Length),
  CounterName = <<"qwemaxlen", BinLength/binary>>,
  max_length_increase(CounterName, Length).

%% @doc Increase the specified counter by one or reset the counter to 0 if the counter maximum bytes length is exceded.
%% Counter length is the number of alphabetic bytes neccessary to display the counter integer value.
max_length_increase(CounterName, Length) when erlang:is_integer(Length) ->

  Value     = increase(CounterName),
  MaxValue  = erlang:list_to_integer(lists:duplicate(Length,$9)),
  case Value > MaxValue of
    true ->
      reset(CounterName),
      %% Avoid concurency reset by increasing the counter again
      max_length_increase(Length, CounterName);
    _ -> Value
  end.