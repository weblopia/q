%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu
%%% @copyright (C) 2017, <Madalin Grigore-Enescu>
%%% @doc
%%%
%%% @end
%%% Created : 07. Feb 2017 6:08 PM
%%%-------------------------------------------------------------------
-module(q_srv_strict_monotonic_time).
-author("Madalin Grigore-Enescu").

-include_lib("q/include/q.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([get_seconds/0]).
-export([get_milliseconds/0]).
-export([get_microseconds/0]).
-export([get_nanoseconds/0]).

%% gen_server Function Exports
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  code_change/3,
  terminate/2
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Creates a gen_server process as part of a supervision tree.
%% The function should be called, directly or indirectly, by the supervisor.
%% It will, among other things, ensure that the gen_server is linked to the supervisor.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Returns the current Q monotonic time in seconds.
%% This is a strictly monotonically increasing time regardless Erlang time warp mode curently in use.
%% That is, consecutive calls to q_lib_time:strict_monotonic_seconds/0 can NOT produce the same result.
get_seconds() -> gen_server:call(?MODULE, get_seconds, infinity).

%% @doc Returns the current Q monotonic time in milliseconds.
%% This is a strictly monotonically increasing time regardless Erlang time warp mode curently in use.
%% That is, consecutive calls to q_lib_time:strict_monotonic_milliseconds/0 can NOT produce the same result.
get_milliseconds() -> gen_server:call(?MODULE, get_milliseconds, infinity).

%% @doc Returns the current Q monotonic time in microseconds
%% This is a strictly monotonically increasing time regardless Erlang time warp mode curently in use.
%% That is, consecutive calls to q_lib_time:strict_monotonic_microseconds/0 can NOT produce the same result.
get_microseconds() -> gen_server:call(?MODULE, get_microseconds, infinity).

%% @doc Returns the current Q monotonic time in nanoseconds
%% This is a strictly monotonically increasing time regardless Erlang time warp mode curently in use.
%% That is, consecutive calls to q_lib_time:strict_monotonic_nanoseconds/0 can NOT produce the same result.
get_nanoseconds() -> gen_server:call(?MODULE, get_nanoseconds, infinity).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INIT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Init gen_server
%% Whenever a gen_server process is started using start/3,4 or start_link/3,4, this function is called by the new process to initialize.
%%
%% Args is the Args argument provided to the start function.
%% If the initialization is successful, the function is to return {ok,State}, {ok,State,Timeout}, or {ok,State,hibernate},
%% where State is the internal state of the gen_server process.
%%
%% If an integer time-out value is provided, a time-out occurs unless a request or a message is received within Timeout milliseconds.
%% A time-out is represented by the atom timeout, which is to be handled by the Module:handle_info/2 callback function.
%% The atom infinity can be used to wait indefinitely, this is the default value.
%%
%% If hibernate is specified instead of a time-out value, the process goes into hibernation when waiting for the next message to arrive
%% (by calling proc_lib:hibernate/3).
%%
%% If the initialization fails, the function is to return {stop,Reason}, where Reason is any term, or ignore.
init([]) ->

  PingTimer	= q_lib_config:get_option(<<"q_srv_strict_monotonic">>),
  erlang:send_after(PingTimer, self(), snapshot),



  State = #q_srv_strict_monotonic_time_state{},

  {ok, State, infinity}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_call
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Handle gen_server calls
%% Whenever a gen_server process receives a request sent using call/2,3 or multi_call/2,3,4, this function is called to handle the request.
%%
%% Request is the Request argument provided to call or multi_call.
%% From is a tuple {Pid,Tag}, where Pid is the pid of the client and Tag is a unique tag.
%% State is the internal state of the gen_server process.
%%
%% If {reply,Reply,NewState} is returned, {reply,Reply,NewState,Timeout} or {reply,Reply,NewState,hibernate},
%% Reply is given back to From as the return value of call/2,3 or included in the return value of multi_call/2,3,4.
%% The gen_server process then continues executing with the possibly updated internal state NewState.
%%
%% For a description of Timeout and hibernate, see Module:init/1.
%%
%% If {noreply,NewState} is returned, {noreply,NewState,Timeout}, or {noreply,NewState,hibernate},
%% the gen_server process continues executing with NewState. Any reply to From must be specified explicitly using reply/2.
%%
%% If {stop,Reason,Reply,NewState} is returned, Reply is given back to From.
%%
%% If {stop,Reason,NewState} is returned, any reply to From must be specified explicitly using reply/2.
%% The gen_server process then calls Module:terminate(Reason,NewState) and terminates.

%% Returns a new monotonic increasing seconds time
handle_call(get_seconds, _From, State = #q_srv_strict_monotonic_time_state{last_seconds = LastTime}) ->

  ErlangSystemTime    = q_lib_time:posix_seconds(),
  StrictMonotonicTime = case ErlangSystemTime > LastTime of
                          true -> ErlangSystemTime;
                          _ -> LastTime+1
                        end,
  {reply, {ok, StrictMonotonicTime, ErlangSystemTime}, State#q_srv_strict_monotonic_time_state{last_seconds = StrictMonotonicTime}};

%% Returns a new monotonic increasing milliseconds time
handle_call(get_milliseconds, _From, State = #q_srv_strict_monotonic_time_state{last_milliseconds = LastTime}) ->

  CurrentTime         = q_lib_time:posix_milliseconds(),
  StrictMonotonicTime = case CurrentTime > LastTime of
                          true -> CurrentTime;
                          _ -> LastTime+1
                        end,
  {reply, {ok, StrictMonotonicTime}, State#q_srv_strict_monotonic_time_state{last_milliseconds = StrictMonotonicTime}};

%% Returns a new monotonic increasing microseconds time
handle_call(get_microseconds, _From, State = #q_srv_strict_monotonic_time_state{last_microseconds = LastTime}) ->

  CurrentTime         = q_lib_time:posix_microseconds(),
  StrictMonotonicTime = case CurrentTime > LastTime of
                          true -> CurrentTime;
                          _ -> LastTime+1
                        end,
  {reply, {ok, StrictMonotonicTime}, State#q_srv_strict_monotonic_time_state{last_microseconds = StrictMonotonicTime}};

%% Returns a new monotonic increasing nanoseconds time
handle_call(get_nanoseconds, _From, State = #q_srv_strict_monotonic_time_state{last_nanoseconds = LastTime}) ->

  CurrentTime         = q_lib_time:posix_nanoseconds(),
  StrictMonotonicTime = case CurrentTime > LastTime of
                          true -> CurrentTime;
                          _ -> LastTime+1
                        end,
  {reply, {ok, StrictMonotonicTime}, State#q_srv_strict_monotonic_time_state{last_nanoseconds = StrictMonotonicTime}};

%% Handle any other call
handle_call(_Msg, _From, State) -> {reply, error, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_cast
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Handle gen_server casts
%% Whenever a gen_server process receives a request sent using cast/2 or abcast/2,3, this function is called to handle the request.
%% For a description of the arguments and possible return values, see Module:handle_call/3.

%% Handle any other cast
handle_cast(_Msg, State) -> {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle_info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Handle gen_server info
%% This function is called by a gen_server process when a time-out occurs or when it receives any other message
%% than a synchronous or asynchronous request (or a system message).
%% Info is either the atom timeout, if a time-out has occurred, or the received message.
%% For a description of the other arguments and possible return values, see Module:handle_call/3.

handle_info(snapshot, State = #q_srv_strict_monotonic_time_state{last_time = LastTime}) ->

  PingTimer	= q_lib_config:get_option(<<"q_srv_strict_monotonic">>),
  erlang:send_after(PingTimer, self(), snapshot),

  {noreply, State};

%% Handle any other info
handle_info(_Info, State) -> {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% terminate/code_change
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Handle gen_server terminate
%%
%% This function is called by a gen_server process when it is about to terminate.
%% It is to be the opposite of Module:init/1 and do any necessary cleaning up.
%% When it returns, the gen_server process terminates with Reason. The return value is ignored.
%%
%% Reason is a term denoting the stop reason and State is the internal state of the gen_server process.
%%
%% Reason depends on why the gen_server process is terminating. If it is because another callback function has returned a stop tuple {stop,..},
%% Reason has the value specified in that tuple. If it is because of a failure, Reason is the error reason.
%%
%% If the gen_server process is part of a supervision tree and is ordered by its supervisor to terminate, this function is called with Reason=shutdown
%% if the following conditions apply:
%% - The gen_server process has been set to trap exit signals.
%% - The shutdown strategy as defined in the child specification of the supervisor is an integer time-out value, not brutal_kill.
%%
%% Even if the gen_server process is not part of a supervision tree, this function is called if it receives an 'EXIT' message from its parent.
%% Reason is the same as in the 'EXIT' message.
%% Otherwise, the gen_server process terminates immediately.
%%
%% Notice that for any other reason than normal, shutdown, or {shutdown,Term}, the gen_server process is assumed to terminate
%% because of an error and an error report is issued using error_logger:format/2.
terminate(_Reason, _State) -> ok.

%% @doc Handle gen_server code change
%% This function is called by a gen_server process when it is to update its internal state during a release upgrade/downgrade,
%% that is, when the instruction {update,Module,Change,...}, where Change={advanced,Extra}, is specifed in the appup file.
%% For more information, see section Release Handling Instructions in OTP Design Principles.
%%
%% For an upgrade, OldVsn is Vsn, and for a downgrade, OldVsn is {down,Vsn}. Vsn is defined by the vsn attribute(s) of the old version
%% of the callback module Module. If no such attribute is defined, the version is the checksum of the Beam file.
%%
%% State is the internal state of the gen_server process.
%% Extra is passed "as is" from the {advanced,Extra} part of the update instruction.
%%
%% If successful, the function must return the updated internal state.
%% If the function returns {error,Reason}, the ongoing upgrade fails and rolls back to the old release.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% actions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Read from storage file
storage_read(State) ->

  {ok, IoDevice}     = file:open(q_lib_paths:get_data_directory(["qwe", "last-strict-monotonic-time"]), [write, read]),
  {ok, _NewPosition} = file:position(IoDevice, {bof, 0}),
  {ok, Data}         = read(IoDevice, Number).
