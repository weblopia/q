%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu
%%% @copyright (C) 2017, <Madalin Grigore-Enescu>
%%% @doc
%%%
%%% @end
%%% Created : 09. Feb 2017 8:38 PM
%%%-------------------------------------------------------------------
-module(q_sup).
-author("madalin").

-behaviour(supervisor).

%% Supervisor exports
-export([start_link/0]).
-export([init/1]).
-export([upgrade/0]).

%% @spec start_link() -> startlink_ret()
%% @doc API for starting the supervisor.
start_link() ->

  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->

  {ok,{{one_for_one, 10, 10}, [SupBatch]}}.

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->

  q_lib_behaviour_supervisor:on_upgrade(?MODULE).