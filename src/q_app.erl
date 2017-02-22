%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu
%%% @copyright (C) 2017, <Madalin Grigore-Enescu>
%%% @doc
%%%
%%% @end
%%% Created : 09. Feb 2017 8:37 PM
%%%-------------------------------------------------------------------
-module(q_app).
-author("madalin").

%% API.
-export([start/2]).
-export([stop/1]).

%% @spec start(Type, Args) -> ServerRet
%% @doc application start callback.
start(_Type, _Args) ->

  %% Check qwe data enviroment configuration parameters
  case application:get_env(qwe, qwe_data_directory) of
    undefined ->

      %% detect qwe data directory
      {file, QweAppModule}  = code:is_loaded(?MODULE),
      RevQweAppModuleDir = lists:reverse(filename:split(QweAppModule)),
      case RevQweAppModuleDir of
        ["ebin", "qwe", "app", _, "releases" | RevQweRoot] ->

          %% check if data directory exists
          QweRoot          = lists:reverse(RevQweRoot),
          QweDataDirectory = filename:append(QweRoot, "data"),
          case filelib:is_dir(QweDataDirectory) of
            true ->


  %% Start application main supervisor
  case q_sup:start_link() of
    {ok, Pid} ->
      {ok, Pid};
    Error ->
      Error
  end.

%% @spec stop(State) -> ServerRet
%% @doc application stop callback.
stop(_State) -> ok.

