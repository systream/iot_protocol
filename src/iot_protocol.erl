%%%-------------------------------------------------------------------
%%% @author Peter Tihanyi
%%% @copyright (C) 2019, systream
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(iot_protocol).

-author("Peter Tihanyi").
-include("iot_protocol.hrl").

%% API
-export([parse/1, assemble/1]).

-callback assemble(iot_protocol_obj()) -> binary().
-callback parse(binary()) -> {ok, iot_protocol_obj()} | chunked_message.

%% @doc Create binary message from iot protocol object.
-spec assemble(iot_protocol_obj()) -> {ok, binary()} | {error, unsupported_version}.
assemble(#iot_protocol{protocol_version = 1} = Obj) ->
  {ok, iot_protocol_v1:assemble(Obj)};
assemble(_Obj) ->
  {error, unsupported_version}.

%% @doc Parse binary message to iot protocol object.
-spec parse(binary()) ->
  {ok, iot_protocol_obj()} | chunked_message | {error, unsupported_version}.
parse(Data) ->
  parse(get_protocol_version(Data), Data).

-spec parse(protocol_version() | chunked_message, binary()) ->
  {ok, iot_protocol_obj()} | chunked_message | {error, unsupported_version}.
parse(chunked_message, _) ->
  chunked_message;
parse(1, Data) ->
  iot_protocol_v1:parse(Data);
parse(_, _Data) ->
  {error, unsupported_version}.

-spec get_protocol_version(binary()) -> protocol_version() | chunked_message.
get_protocol_version(<<Size:?PAYLOAD_LENGTH_SIZE/integer,
                       ProtocolVersion:?PROTOCOL_VERSION_SIZE/integer,
                       _/binary>>) when Size =< ?MAX_MSG_SIZE  ->
  ProtocolVersion;
get_protocol_version(<<Size:?PAYLOAD_LENGTH_SIZE/integer,
                       _/binary>>) when Size > ?MAX_MSG_SIZE  ->
  throw("Payload cannot be more thant 4Gbyte");
get_protocol_version(_) ->
  chunked_message.