%%%-------------------------------------------------------------------
%%% @author madalin
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Feb 2017 7:43 PM
%%%-------------------------------------------------------------------
-module(q_lib_universe).
-author("madalin").

%% API
-export([get_local_cluster/0]).
-export([get_local_qnode/0]).

-export([get_all_clusters/0]).
-export([get_connected_clusters/0]).
-export([get_disconected_clusters/0]).

-export([get_all_qnodes/0]).
-export([get_connected_qnodes/0]).
-export([get_disconnected_qnodes/0]).

%% @doc Returns local cluster
get_local_cluster() -> q_lib_config:get_option(<<"cluster">>).

%% @doc Returns Q local node
%% Please notice a Q node is diffrent than a native erlang node.
%% A Q node include the cluster id and the erlang node full name meaningfull on the specified cluster.
get_local_qnode() -> q_lib_config:get_option(<<"qnode">>).

%% @doc Returns all clusters
get_all_clusters() -> q_lib_config:get_option(<<"all_clusters">>).

%% @doc Returns connected clusters
get_connected_clusters() -> q_lib_config:get_option(<<"connected_clusters">>).

%% @doc Returns disconected clusters
get_disconected_clusters() -> q_lib_config:get_option(<<"connected_clusters">>).
