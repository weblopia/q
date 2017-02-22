%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu
%%% @copyright (C) 2017, <Madalin Grigore-Enescu>
%%% @doc
%%%
%%% @end
%%% Created : 19. Feb 2017 7:55 AM
%%%-------------------------------------------------------------------
-module(q_lib_config).
-author("madalin").

-include_lib("q/include/q.hrl").

%% API
-export([get_option/1, get_option/2]).
-export([set_option/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% get
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec get_option(Name :: binary()) -> term()
%% @doc Return the specified configuration option value or raise exception if the specified configuration option was not found.
%% @throws {qwe_missing_config_option, Name::binary()}
get_option(Name) when erlang:is_binary(Name) ->

  case ets:lookup(?Q_ETS_TABLE_CONFIG, Name) of
    [{Name, Value}] -> Value;
    _ -> throw({qwe_missing_config_option, Name})
  end.

%% @spec get_option(Name :: binary(), Default :: term()) -> term() | Default
%% @doc Return the specified config option value if exists, otherwise returns Default parameter.
get_option(Name, Default) when erlang:is_binary(Name) ->

  Result  = ets:lookup(?Q_ETS_TABLE_CONFIG, Name),
  case Result of
    [{Name, Value}] -> Value;
    _ -> Default
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% set
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec set_option(Name :: binary(), Value :: term()) -> true
%% @doc Set a configuration option value. If exists the old configuration option is replaced.
set_option(Name, Value) -> ets:insert(?Q_ETS_TABLE_CONFIG, {Name, Value}).