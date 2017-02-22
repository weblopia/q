%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu
%%% @copyright (C) 2017, <Madalin Grigore-Enescu>
%%% @doc
%%% Implements most used universally unique identifier (UUID) standards: V1, V3, V4 and V5
%%% @end
%%% Created : 19. Feb 2017 7:29 PM
%%%-------------------------------------------------------------------
-module(q_lib_uuid).
-author("madalin").

-include_lib("q/include/q.hrl").

%% API
-export([nil/0]).

-export([new_v1/0]).
-export([is_valid_v1/1]).
-export([is_valid_v1_guard/1]).

-export([new_v3/1, new_v3/2]).
-export([is_valid_v3/1]).
-export([is_valid_v3_guard/1]).

-export([new_v4/0]).
-export([is_valid_v4/1]).
-export([is_valid_v4_guard/1]).

-export([new_v5/0]).
-export([is_valid_v5/1]).
-export([is_valid_v5_guard/1]).

-export([format_canonical/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% nil
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns the NIL UUID.
%% The "nil" UUID, a special case, is the UUID, 00000000-0000-0000-0000-000000000000; that is, all bits set to zero.
nil() -> <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% v1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Generate a new version 1 UUID
%% Version 1 concatenates the 48-bit MAC address of the "node" (that is, the computer generating the UUID),
%% with a 60-bit timestamp, being the number of 100-nanosecond intervals since midnight 15 October 1582 Coordinated Universal Time (UTC),
%% the date on which the Gregorian calendar was first adopted.
%%
%% RFC 4122 states that the time value rolls over around 3400 AD, depending on the algorithm used,
%% which implies that the 60-bit timestamp is a signed quantity. However some software, such as the libuuid library,
%% treats the timestamp as unsigned, putting the rollover time in 5236 AD.
%%
%% A 14-bit "uniqifying" clock sequence extends the timestamp in order to handle cases where the processor clock does not advance fast enough,
%% or where there are multiple processors and UUID generators per node. With each version 1 UUID corresponding to a single point in space
%% (the node) and time (intervals and clock sequence), the chance of two properly-generated version 1 UUID's being unintentionally the same is practically nil.
%%
%% Since the time and clock sequence total 74 bits, 274 (1.8x1022 or 18 sextillion) version 1 UUIDs can be generated per node id,
%% at a maximum average rate of 163 billion per second per node id.
%%
%% In contrast to other UUID versions, version 1 and 2 UUIDs based on MAC addresses from network cards rely for their uniqueness in part on an identifier
%% issued by a central registration authority, namely the Organizationally Unique Identifier (OUI) part of the MAC address, which is issued by the IEEE
%% to manufacturers of networking equipment.
%%
%% Usage of the the node's network card MAC address for the node id means that a version 1 UUID can be tracked back to the computer that created it.
%% Documents can sometimes be traced to the computers where they were created or edited through UUIDs embedded into them by word processing software.
%% This privacy hole was used when locating the creator of the Melissa virus.
%%
%% RFC 4122 does allow the MAC address in a version 1 (or 2) UUID to be replaced by a random 48-bit node id,
%% either because the node does not have a MAC address, or because it is not desirable to expose it.
%%
%% In that case, the RFC requires that the least significant bit of the first octet of the node id should be set to 1.
%% This corresponds to the multicast bit in MAC addresses and setting it serves to differentiate UUIDs where the node id
%% is randomly-generated from those based on MAC addresses from network cards, which typically have unicast MAC addresses.
new_v1() ->

  Nanoseconds   = q_lib_time:posix_nanoseconds(),
  Time          = Nanoseconds + 122192928000000000,
  <<TimeHigh:12, TimeMid:16, TimeLow:32>> = <<Time:60>>,
  ClockSequence = q_lib_node_counters:max_length_increase(16383),
  <<NodeIdHashFragment:47/unit:1, _:1/unit:1, _/binary>> = crypto:hash(sha, erlang:atom_to_binary(node(), utf8)),

  <<TimeLow:32, TimeMid:16, 0:1, 0:1, 0:1, 1:1, TimeHigh:12, 1:1, 0:1, ClockSequence:14, NodeIdHashFragment:47/unit:1, 1:1/unit:1>>.

%% @spec is_valid_v1(Uuid :: term()) -> true
%% @doc Returns true if the specified term is a valid UUID V1 binary or false otherwise.
is_valid_v1(<<_TimeLow:32, _TimeMid:16, 0:1, 0:1, 0:1, 1:1, _TimeHigh:12, 1:1, 0:1, _ClockSequence:14, _Unique:48>>) -> true;
is_valid_v1(_) -> false.

%% @spec is_valid_v1_guard(UuidMi :: term()) -> true
%% @doc Guard the specified UUID V1 for validity
is_valid_v1_guard(Term) -> true = is_valid_v1(Term).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% v3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Generate a new version 3 uuid
%% Version 3 UUIDs are generated by hashing a namespace identifier and name. Version 3 uses MD5 as the hashing algorithm.
%% The namespace identifier is itself a UUID. The specification provides UUIDs to represent the namespaces for URLs,
%% fully qualified domain names, object identifiers, and X.500 distinguished names; but other UUIDs may be used as namespace designators.
%% To determine the version 3 UUID corresponding to a given namespace and name, the UUID of the namespace is transformed to a string of bytes,
%% concatenated with the input name, then hashed with MD5, yielding 128 bits.
%% Six bits are then replaced by fixed values, the 4-bit version (e.g. 0011 for version 3), and the 2-bit UUID "variant" (e.g 10 indicating RFC 4122 UUIDs).
%% Since 6 bits are thus predetermined, only 122 bits contribute to the uniqueness of the UUID.
%% Version 3 UUIDs have the property that the same namespace and name will map to the same UUID.
%% However, the namespace and name cannot be determined from the version 3 UUID alone.
%%
%% RFC 4122 recommends version 5 (SHA1) over version 3 (MD5) and counsels against use of UUIDs of either version as security credentials.
new_v3(Name) when is_binary(Name) ->
  <<B1:48, _:4, B2:12, _:2, B3:14, B4:48>> = crypto:hash(md5, Name),
  <<B1:48, 0:1, 0:1, 1:1, 1:1, B2:12, 1:1, 0:1, B3:14, B4:48>>.

%% @spec new_v3(Namespace :: url | fqdn | oi | x500 | binary()) -> binary()
new_v3(url, Name) -> new_v3(?Q_UUID_NAMESPACE_URL, Name);
new_v3(fqdn, Name) -> new_v3(?Q_UUID_NAMESPACE_FQDN, Name);
new_v3(oi, Name) -> new_v3(?Q_UUID_NAMESPACE_OI, Name);
new_v3(x500, Name) -> new_v3(?Q_UUID_NAMESPACE_X500, Name);
new_v3(Namespace, Name) when
  is_binary(Namespace),
  erlang:is_binary(Name) ->
  new_v3(<<Namespace/binary, Name/binary>>).

%% @spec is_valid_v3(Uuid :: term()) -> true
%% @doc Returns true if the specified term is a valid UUID V3 binary or false otherwise.
is_valid_v3(<<_:48, 0:1, 0:1, 1:1, 1:1, _:12, 1:1, 0:1, _:62>>) -> true;
is_valid_v3(_) -> false.

%% @spec is_valid_v3(UuidMi :: term()) -> true
%% @doc Guard the specified UUID V3 for validity
is_valid_v3_guard(Term) -> true = is_valid_v3(Term).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% v4
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Generate a new version 4 UUID
new_v4() -> throw(qwe_unimplemented).

%% @spec is_valid_v4(Uuid :: term()) -> true
%% @doc Returns true if the specified term is a valid UUID V4 binary or false otherwise.
is_valid_v4(<<_:48, 0:1, 1:1, 0:1, 0:1, _:12, 1:1, 0:1, _:62>>) -> true;
is_valid_v4(_) -> false.

%% @spec is_valid_v4_guard(UuidMi :: term()) -> true
%% @doc Guard the specified UUID V4 for validity
is_valid_v4_guard(Term) -> true = is_valid_v4(Term).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% v5
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Generate a new version 5 UUID
new_v5() -> throw(qwe_unimplemented).

%% @spec is_valid_v5(Uuid :: term()) -> true
%% @doc Returns true if the specified term is a valid UUID V5 binary or false otherwise.
is_valid_v5(<<_:48, 0:1, 1:1, 0:1, 0:1, _:12, 1:1, 0:1, _:62>>) -> true;
is_valid_v5(_) -> false.

%% @spec is_valid_v5_guard(UuidMi :: term()) -> true
%% @doc Guard the specified UUID V5 for validity
is_valid_v5_guard(Term) -> true = is_valid_v5(Term).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% format
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Format a 128 bit uuid in its canonical textual representation.
%%
%% In its canonical textual representation, the sixteen octets of a UUID are represented as 32 lowercase hexadecimal (base 16) digits,
%% displayed in five groups separated by hyphens, in the form 8-4-4-4-12 for a total of 36 characters (32 alphanumeric characters and four hyphens).
%%
%% For example:
%% 123e4567-e89b-12d3-a456-426655440000
%% xxxxxxxx-xxxx-Mxxx-Nxxx-xxxxxxxxxxxx
%%
%% The one to three most significant bits of digit N indicate the UUID variant and the four bits of digit M indicate the UUID version.
%% In the example, M is 1 and N is a (10xx), meaning that the UUID is a variant 1, version 1 UUID; that is, a time-based DCE/RFC 4122 UUID.
format_canonical(<<C1:4/binary, C2:2/binary, C3:2/binary, C4:2/binary, C5:6/binary>>) ->
  <<(q_lib_binaries:convert_to_hex(C1, lower))/binary, $-,
  (q_lib_binaries:convert_to_hex(C2, lower))/binary, $-,
  (q_lib_binaries:convert_to_hex(C3, lower))/binary, $-,
  (q_lib_binaries:convert_to_hex(C4, lower))/binary, $-,
  (q_lib_binaries:convert_to_hex(C5, lower))/binary>>.
