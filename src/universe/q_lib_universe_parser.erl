%%%-------------------------------------------------------------------
%%% @author madalin
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Feb 2017 6:21 PM
%%%-------------------------------------------------------------------
-module(q_lib_universe_parser).
-author("madalin").

%% API
-export([new/0]).
-export([parse_new/1]).
-export([parse_more/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% new
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Initialize a new parser
%% Returns the parsing fun
new() -> fun(Bin) -> parse_new(Bin) end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parse_new
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Parse a new incoming packet

%% Parse empty data
parse_new(<<>>) -> {more, fun(Bin) -> parse_new(Bin) end};

%% Parse a full packet.
parse_new(<<SizeOfSize:8/big-unsigned-integer, Size:SizeOfSize/big-unsigned-integer, Content:Size/binary, Rest/binary>>) ->

  PacketDecoderResult = q_lib_universe_packet:decode(Content),
  case PacketDecoderResult of
    {ok, Decoded} -> {ok, Decoded, Rest};
    _ -> PacketDecoderResult
  end;

%% Parse a partial packet with full size header.
%% Returns a new parser to the caller that expects the rest of the packet to come from the stream
parse_new(<<SizeOfSize:8/big-unsigned-integer, Size:SizeOfSize/big-unsigned-integer, Rest/binary>>) ->

  RemainingSize = Size - erlang:size(Rest),
  {more, fun(Bin) -> parse_more(RemainingSize, Bin, Rest) end};

%% Parse a partial packet that have incomplete full size header.
%% Returns to caller a new parser that expects more size information from stream
parse_new(SizeFragment) -> {more, fun(Bin) -> parse_new(<<SizeFragment/binary, Bin/binary>>) end}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parse_more
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Parse the rest of the packet

%% Ignore empty data
parse_more(RemainingSize, <<>>, LastBin) ->

  {more, fun(Bin) -> parse_more(RemainingSize, Bin, LastBin) end};

%% Parse complete packet from data
parse_more(RemainingSize, <<Content:RemainingSize/binary, Rest/binary>>, LastBin) ->

  PacketDecoderResult = q_lib_universe_packet:decode(<<LastBin/binary, Content/binary>>),
  case PacketDecoderResult of
    {ok, Decoded} -> {ok, Decoded, Rest};
    _ -> PacketDecoderResult
  end;

%% Parse more partial content from current packet
parse_more(RemainingSize, More, LastBin) ->

  NewRemainingSize = RemainingSize - erlang:size(More),
  {more, fun(Bin) -> parse_more(NewRemainingSize, Bin, <<LastBin/binary, More/binary>>) end}.