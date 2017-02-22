%%%-------------------------------------------------------------------
%%% @author madalin
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%% Functions to handle the directory structure used by QWE framework.
%%%
%%% Below is the standard directory structure of a QWE project:
%%%
%%% /projectname
%%%             bin/
%%%             config/
%%%             data/
%%%             rel/
%%%                 version/
%%%                         app/
%%%                         assets/
%%%                         bin/
%%%                         erlang/
%%%
%%% @end
%%% Created : 22. Feb 2017 12:43 PM
%%%-------------------------------------------------------------------
-module(q_lib_paths).
-author("madalin").

%% API
-export([module_directory/1]).
-export([module_application_directory/1]).
-export([application_directory/1]).

-export([project_directory/0, project_directory/1]).
-export([project_data_directory/0, project_data_directory/1]).
-export([project_temp_directory/0]).
-export([project_temp_persistent_directory/0]).
-export([project_config_directory/0]).

-export([release_directory/0, release_directory/1]).
-export([release_bin_directory/0]).
-export([release_assets_directory/0]).
-export([release_applications_directory/0]).

%% @spec module(Module) -> {ok, string()} | {error, not_found}
%% @doc Returns the specified module directory.
module_directory(Name) ->
    case code:is_loaded(Name) of
        {file, Here} -> {ok, filename:dirname(Here)};
        _ -> {error, not_found}
    end.

%% @spec module(Module) -> {ok, string()} | {error, not_found} | {error, unknown_directory_structure}
%% @doc Returns the specified module application directory.
module_application_directory(Name) ->
    case module_directory(Name) of
        {ok, ApplicationEbinDirectory} -> {ok, filename:dirname(ApplicationEbinDirectory)};
        Error -> Error
    end.

%% @spec application(Module) -> {ok, string()} | {error, not_found}
%% @doc Returns the specified application directory (including application name).
application_directory(Name) ->

    case code:where_is_file([Name | ".app"]) of
        non_existing -> {error, not_found};
        Filename -> {ok, filename:dirname(filename:dirname(Filename))}
    end.

%% @doc Returns project data directory
project_directory() ->

    case application:get_env(q, q_path_project) of
        {ok, Directory} -> Directory
    end.

%% @doc Returns project directory joined with the specified components
project_directory(Components) -> filename:join([project_directory() | Components]).

%% @doc Returns project data directory
%% Data directory is outside current release directory and can be shared by diffrent releases.
%% This allow you to upgrade your project without losing data files.
project_data_directory() ->

    case application:get_env(q, q_path_project_data) of
        {ok, Directory} -> Directory;
        _ -> filename:join([project_directory() | ["data"]])
    end.

%% @doc Returns project data directory joined with the specified components
project_data_directory(Components) -> filename:join([project_data_directory() | Components]).

%% @doc Returns project temporary directory
%% Returns a directory where files are most likely NOT preserved after project restart.
%% If you want to get a temporary directory where files MAY be preserved between project restart
%% use q_lib_paths:project_temp_persistent_directory/0
project_temp_directory() ->

    case application:get_env(q, q_project_temp_directory) of
        {ok, Directory} -> Directory;
        _ -> "/tmp"
    end.

%% @doc Returns project temporary persistent directory
%% Returns a directory where files MAY be preserved after project restart.
project_temp_persistent_directory() ->

    case application:get_env(q, q_project_temp_persistent_directory) of
        {ok, Directory} -> Directory;
        _ -> "/var/tmp"
    end.

%% @doc Returns project configuration directory
%% Config directory is outside current release directory and can be shared by diffrent releases.
%% This allow you to upgrade your project without losing configuration files.
project_config_directory() ->

    case application:get_env(q, q_path_project_config) of
        {ok, Directory} -> Directory;
        _ -> filename:join([project_directory() | ["config"]])
    end.

%% @doc Returns project current release directory
release_directory() ->

    case application:get_env(q, q_path_project_current_release) of
        {ok, Directory} -> Directory
    end.

%% @doc Returns project current release directory joined with the specified components
release_directory(Components) -> filename:join([release_directory() | Components]).

%% @doc Returns project release binaries directory
%% Data directory is outside current release directory and can be shared by diffrent releases.
%% This allow you to upgrade your project without losing data files.
release_bin_directory() -> release_directory(["bin"]).

%% @doc Returns project release assets directory
%% Config directory is outside current release directory and can be shared by diffrent releases.
%% This allow you to upgrade your project without losing configuration files.
release_assets_directory() -> release_directory(["assets"]).

%% @doc Returns project release assets directory
%% Config directory is outside current release directory and can be shared by diffrent releases.
%% This allow you to upgrade your project without losing configuration files.
release_applications_directory() -> release_directory(["app"]).


