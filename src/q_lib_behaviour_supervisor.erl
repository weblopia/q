%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu
%%% @copyright (C) 2017, <Madalin Grigore-Enescu>
%%% @doc
%%%
%%% @end
%%% Created : 09. Feb 2017 8:39 PM
%%%-------------------------------------------------------------------
-module(q_lib_behaviour_supervisor).
-author("madalin").

-include_lib("q/include/q.hrl").

%% API
-export([on_upgrade/1]).

%% @spec on_upgrade() -> ok
%% @doc Helper function to call on supervisor upgrade
%% This method retrieve supervisor specifications after upgrade, terminate existing children and start them again.
on_upgrade(Module) ->

  %% Retrieve specifications again
  {ok, {_, Specs}} = Module:init([]),

  %% Compute the list of supervisor children to terminate
  Old  = sets:from_list([Name || {Name, _, _, _} <- supervisor:which_children(Module)]),
  New  = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
  Kill = sets:subtract(Old, New),

  %% Terminate all children
  sets:fold(fun (Id, ok) ->
    supervisor:terminate_child(Module, Id),
    supervisor:delete_child(Module, Id),
    ok
  end, ok, Kill),

  %% Restart all supervisor children
  [supervisor:start_child(Module, Spec) || Spec <- Specs],

  ok.