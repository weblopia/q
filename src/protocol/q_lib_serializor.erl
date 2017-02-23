%%%-------------------------------------------------------------------
%%% @author madalin
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% This module contains functions to serialize and deserialize erlang terms using a special format
%%% called QE, created for use in communication protocols with other programming languages.
%%%
%%% Because of this during serialization process, some specific native Erlang data types will be converted to more
%%% generic data types supported by most programming languages. Also pleas notice because of this When deserializing
%%% data the original data type may NOT be preserved.
%%%
%%% The following list contains all the QSER data types:
%%% -
%%%
%%% The following table describe the conversion performed by QSER from native data types to QSER data types:
%%% ___________________________________
%%% | Erlang type | Generic data type |
%%% -----------------------------------
%%% | utf8 string       |
%%% | atom        | utf8 string       |
%%% -----------------------------------
%%%
%%% The following table describe the conversion performed by QSER from QSER data types to native data types when deserializing data:
%%%
%%% The QSER format was designed to be lightweight and easy to implement in other programming languages.
%%%
%%% Despite the fact QSER is very lightweight we didn't impose any limitation on the size of the data we can serialize.
%%% Imposing limitations to the data size can be implemented as a protocol implementation specification.
%%%
%%%
%%% @end
%%% Created : 22. Feb 2017 6:46 PM
%%%-------------------------------------------------------------------
-module(q_lib_serializor).

-include_lib("q/include/q.hrl").

%% API
-export([encode/1]).

-export([encode_binary/1]).
-export([encode_utf8/1]).
-export([encode_int/1]).
-export([encode_term/1]).

-export([decode_binary/1]).
-export([decode_utf8/1]).
-export([decode_int/1]).
-export([decode_term/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% encode
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Encode the specified term
encode(Term) when erlang:is_binary(Term) -> encode_binary(Term);
encode(Term) when erlang:is_integer(Term) -> encode_integer(Term);
encode(Term) when erlang:is_list(Term) -> encode_list(Term);
encode(Term) when erlang:is_tuple(Term) -> encode_tuple(Term);
encode(Term) -> encode_term_to_binary(Term).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% binary
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Encode list or binary to encoded binary
encode_binary(undefined) -> <<0:16/big-integer>>;
encode_binary(Term) when erlang:is_list(Term) -> encode_binary(list_to_binary(Term));
encode_binary(Term) when erlang:is_binary(Term) ->
  Length = byte_size(Term),
  true   = (Length =< 16#ffff),
  <<Length:16/big-integer, Term:Length/binary>>.

%% @doc Decode encoded binary
decode_binary(<<Length:16/big-unsigned-integer, Binary:Length/binary, Rest/binary>>) -> {Binary, Rest};
decode_binary(_Any) -> error.

%% @doc Decode next encoded binary adding it to acummulator
decode_next_binary(<<Length:16/big-unsigned-integer, Binary:Length/binary, Rest/binary>>) -> {Rest, [Binary]};
decode_next_binary({<<Length:16/big-unsigned-integer, Binary:Length/binary, Rest/binary>>, Acum}) when erlang:is_list(Acum) ->
  NewAcum = lists:append(Acum, [Binary]),
  {Rest, NewAcum};
decode_next_binary(_Any) -> error.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% encoded UTF-8 string
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Encode list or binary to encoded UTF-8 string
encode_utf8(undefined) -> <<0:16/big-integer>>;
encode_utf8(Term) ->
  StringBin = unicode:characters_to_binary(Term),
  Length    = size(StringBin),
  true      = (Length =< 16#ffff),
  <<Length:16/big-integer, StringBin:Length/binary>>.

%% @doc Decode encoded UTF-8 string
decode_utf8(<<Length:16/big-unsigned-integer, Binary:Length/binary, Rest/binary>>) -> {Binary, Rest};
decode_utf8(_Any) -> error.

%% @doc Decode next encoded UTF-8 string adding it to acummulator
decode_next_utf8(<<Length:16/big-unsigned-integer, Binary:Length/binary, Rest/binary>>) -> {Rest, [Binary]};
decode_next_utf8({<<Length:16/big-unsigned-integer, Binary:Length/binary, Rest/binary>>, Acum}) when erlang:is_list(Acum) ->
  NewAcum = lists:append(Acum, [Binary]),
  {Rest, NewAcum};
decode_next_utf8(_Any) -> error.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 8uint
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Encode integer to 8uint
encode_8uint(undefined) -> <<0:8/big-unsigned-integer>>;
encode_8uint(Uint) when erlang:is_integer(Uint) -> <<Uint:8/big-unsigned-integer>>.

%% @doc Decode 8uint to integer
decode_8uint(<<Uint:8/big-unsigned-integer, Rest/binary>>) -> {Uint, Rest};
decode_8uint(_Any) -> error.

%% @doc Decode next 8uint to integer adding it to acumulator
decode_next_8uint(<<Uint:8/big-unsigned-integer, Rest/binary>>) -> {Rest, [Uint]};
decode_next_8uint({<<Uint:8/big-unsigned-integer, Rest/binary>>, Acum}) when erlang:is_list(Acum) ->
  NewAcum = lists:append(Acum, [Uint]),
  {Rest, NewAcum};
decode_next_8uint(_Any) -> error.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 16uint
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Encode integer to 16uint
encode_16uint(undefined) -> <<0:16/big-integer>>;
encode_16uint(Uint) when erlang:is_integer(Uint) -> <<Uint:16/big-integer>>.

%% @doc Decode 16uint to integer
decode_16uint(<<Uint:16/big-unsigned-integer, Rest/binary>>) -> {Uint, Rest};
decode_16uint(_Any) -> error.

%% @doc Decode next 16uint to integer adding it to acumulator
decode_next_16uint(<<Uint:16/big-unsigned-integer, Rest/binary>>) -> {Rest, [Uint]};
decode_next_16uint({<<Uint:16/big-unsigned-integer, Rest/binary>>, Acum}) when erlang:is_list(Acum) ->
  NewAcum = lists:append(Acum, [Uint]),
  {Rest, NewAcum};
decode_next_16uint(_Any) -> error.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 32uint
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Encode integer to 32uint
encode_32uint(undefined) -> <<0:32/big-integer>>;
encode_32uint(Uint) when erlang:is_integer(Uint) -> <<Uint:32/big-integer>>.

%% @doc Decode 32uint to integer
decode_32uint(<<Uint:32/big-unsigned-integer, Rest/binary>>) -> {Uint, Rest};
decode_32uint(_Any) -> error.

%% @doc Decode next 32uint to integer adding it to acumulator
decode_next_32uint(<<Uint:32/big-unsigned-integer, Rest/binary>>) -> {Rest, [Uint]};
decode_next_32uint({<<Uint:32/big-unsigned-integer, Rest/binary>>, Acum}) when erlang:is_list(Acum) ->
  NewAcum = lists:append(Acum, [Uint]),
  {Rest, NewAcum};
decode_next_32uint(_Any) -> error.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 64uint
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Encode integer to 64uint
encode_64uint(undefined) -> <<0:64/big-unsigned-integer>>;
encode_64uint(Uint) when erlang:is_integer(Uint) -> <<Uint:64/big-unsigned-integer>>.

%% @doc Decode 64uint to integer
decode_64uint(<<Uint:64/big-unsigned-integer, Rest/binary>>) -> {Uint, Rest};
decode_64uint(_Any) -> error.

%% @doc Decode next 64uint to integer adding it to acumulator
decode_next_64uint(<<Uint:64/big-unsigned-integer, Rest/binary>>) -> {Rest, [Uint]};
decode_next_64uint({<<Uint:64/big-unsigned-integer, Rest/binary>>, Acum}) when erlang:is_list(Acum) ->
  NewAcum = lists:append(Acum, [Uint]),
  {Rest, NewAcum};
decode_next_64uint(_Any) -> error.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% decode
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec decode(term()) -> {ok, term(), binary()} | error.
%% @doc Decode generic Q protocol packet
decode(<<PacketId:8, Rest/binary>>) -> decode(PacketId, Rest);
decode(_PacketBinary) -> error.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% encode
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec encode(tuple()) -> {ok, term()} | error.
%% @doc Encode packet
encode(PacketRecord) ->

  %% Encode content
  case encode_content(PacketRecord) of

    {ok, BinaryContent} ->

      %% Compute content size
      Size = erlang:size(BinaryContent),

      %% Add header and return
      {ok, <<Size:16/big-integer, BinaryContent/binary>>};

    _ -> error

  end.