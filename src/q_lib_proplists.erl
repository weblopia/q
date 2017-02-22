%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu
%%% @copyright (C) 2017, <Madalin Grigore-Enescu>
%%% @doc
%%%
%%% @end
%%% Created : 07. Feb 2017 6:01 PM
%%%-------------------------------------------------------------------
-module(q_lib_proplists).
-author("madalin").

-include_lib("q/include/q.hrl").

%% API
-export([ensure_properties/2]).

%% @doc Ensure the specified elements exists into the specified proplist
ensure_properties([], Proplist) -> Proplist;
ensure_properties([{K, V}|T], Proplist) ->
  case proplists:is_defined(K, Proplist) of
    true -> ensure_properties(T, Proplist);
    _ -> ensure_properties(T, lists:append(Proplist, [{K,V}]))
  end.




