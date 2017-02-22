%%%-------------------------------------------------------------------
%%% @author madalin
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Feb 2017 6:10 PM
%%%-------------------------------------------------------------------
-author("madalin").

%% ETS table holding configuration options
-define(Q_ETS_TABLE_CONFIG,                 qwe_ets_table_config).

%% ETS table holding counters
-define(Q_ETS_TABLE_COUNTERS,               qwe_ets_table_counters).

%% Seconds coresponding to the Q epoch on January 1st, 2017 at UTC
%% 1483228800 seconds from Unix epoch
-define(Q_TIME_SECONDS_Q_EPOCH,             1483228800).

%% Milliseconds coresponding to the Q epoch on January 1st, 2017 at UTC
-define(Q_TIME_MILLISECONDS_Q_EPOCH,        1483228800000).

%% Microseconds coresponding to the Q epoch on January 1st, 2017 at UTC
-define(Q_TIME_MICROSECONDS_Q_EPOCH,        1483228800000000).

%% Nanoseconds coresponding to the Q epoch on January 1st, 2017 at UTC
-define(Q_TIME_NANOSECONDS_Q_EPOCH,         1483228800000000000).

%% Datetime coresponding to the Q epoch on January 1st, 2017 at UTC
-define(Q_DATETIME_Q_EPOCH,                 {{2017, 1, 1}, {0, 0, 0}}).

%% Seconds coresponding to the Unix Epoch on January 1st, 1970 at UTC
%% This value is returned by calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})
-define(Q_TIME_SECONDS_UNIX_EPOCH,          62167219200).

%% Standard uuid namespaces
-define(Q_UUID_NAMESPACE_URL,               <<16#6ba7b8119dad11d180b400c04fd430c8:128>>).
-define(Q_UUID_NAMESPACE_FQDN,              <<16#6ba7b8109dad11d180b400c04fd430c8:128>>).
-define(Q_UUID_NAMESPACE_OI,                <<16#6ba7b8129dad11d180b400c04fd430c8:128>>).
-define(Q_UUID_NAMESPACE_X500,              <<16#6ba7b8149dad11d180b400c04fd430c8:128>>).

%% Default uuidimi namespace
-define(Q_UUIDIMI_DEFAULT_NAMESPACE,        <<"q">>).

%% Maximum uuidimi nanoseconds component length.
-define(Q_UUIDIMI_MAX_NANOSECONDS,          999999999999999999).

%% Maximum uuidimi nanoseconds component length.
-define(Q_UUIDIMI_MAX_NANOSECONDS_LENGTH,   18).

%% Maximum time uuid unique component bytes length.
-define(Q_UUIDIMI_MAX_UNIQUE_LENGTH,        18).

%% Fixed bytes length of a time uuid.
-define(Q_UUIDIMI_LENGTH,                   36).

-record(q_sc_node, {
  cloud_id = undefined,
  node_id = undefined
}).

-record(q_sc_pid, {
  cloud_id = undefined,
  node_id = undefined,
  pid = undefined
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Q gen_server states
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(q_srv_ping_state, {
}).

-record(q_srv_strict_monotonic_time_state, {
  file = undefined :: undefined | io_device(),
  last_seconds = undefined :: undefined | pos_integer(),
  last_milliseconds = undefined :: undefined | pos_integer(),
  last_microseconds = undefined :: undefined | pos_integer(),
  last_nanoseconds = undefined :: undefined | pos_integer()
}).

