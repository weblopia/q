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
-export([get_node_option/1, get_node_option/2]).
-export([get_cluster_option/1, get_cluster_option/2]).
-export([get_universe_option/1, get_universe_option/2]).

-export([set_node_option/2]).
-export([set_cluster_option/2]).
-export([set_universe_option/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% get
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec get_option(Name :: binary()) -> term()
%% @doc Return the specified configuration option value or raise exception if the specified configuration option is not availlable.
%% @throws {missing_config_option, Name::binary()}
get_option(Name) when erlang:is_binary(Name) ->

  case q_srv_config:get_option(Name) of
    {ok, Value} -> Value;
    _ -> throw({config_option_not_availlable, Name})
  end.

%% @spec get_option(Name :: binary(), Default :: term()) -> term() | Default
%% @doc Return the specified config option value if it's availlable, otherwise returns Default parameter.
get_option(Name, Default) when erlang:is_binary(Name) ->

  case q_srv_config:get_option(Name) of
    {ok, Value} -> Value;
    _ -> Default
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% set
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec set_option(Name :: binary(), Value :: term()) -> true
%% @doc Set a configuration option value. If exists the old configuration option is replaced.
set_option(Name, Value) ->

  ets:insert(?Q_ETS_TABLE_CONFIG, {Name, Value}).
