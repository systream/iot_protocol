%%%-------------------------------------------------------------------
%%% @author tihanyipeter
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Sep 2019 21:26
%%%-------------------------------------------------------------------
-module(iot_protocol_obj).
-author("tihanyipeter").

-include("iot_protocol.hrl").

%% API
-export([new/2,
  new/1,
  new/3,
  get_payload/1,
  get_protocol_version/1,
  get_message_type/1,
  get_total_message_length/1,
  get_audit_number/1,
  set_audit_number/2]).

-spec new(integer()) -> iot_protocol_obj().
new(MessageType) ->
  new(?LATEST_PROTOCOL_VERSION, MessageType, <<>>).

-spec new(integer(), integer() | binary) -> iot_protocol_obj().
new(MessageType, <<Payload/binary>>) when is_integer(MessageType) ->
  new(?LATEST_PROTOCOL_VERSION, MessageType, Payload);
new(ProtocolVersion, MessageType) when is_integer(ProtocolVersion) andalso is_integer(MessageType) ->
  new(ProtocolVersion, MessageType, <<>>).

-spec new(integer(), integer(), binary()) -> iot_protocol_obj().
new(ProtocolVersion, MessageType, <<Payload/binary>>)
  when is_integer(ProtocolVersion) andalso is_integer(MessageType) ->
  #iot_protocol{
    protocol_version = ProtocolVersion,
    message_type = MessageType,
    payload = Payload,
    total_message_length = byte_size(Payload) + 11
  }.

-spec get_protocol_version(iot_protocol_obj()) -> integer().
get_protocol_version(#iot_protocol{protocol_version = ProtocolVersion}) ->
  ProtocolVersion.

-spec get_message_type(iot_protocol_obj()) -> integer().
get_message_type(#iot_protocol{message_type = MessageType}) ->
  MessageType.

-spec get_total_message_length(iot_protocol_obj()) -> integer() | unknown.
get_total_message_length(#iot_protocol{total_message_length = MsgLength}) ->
  MsgLength.

-spec get_payload(iot_protocol_obj()) -> binary().
get_payload(#iot_protocol{payload = Payload}) ->
  Payload.

-spec get_audit_number(iot_protocol_obj()) -> integer().
get_audit_number(#iot_protocol{audit_number = AuditNumber}) ->
  AuditNumber.

-spec set_audit_number(iot_protocol_obj(), integer()) -> iot_protocol_obj().
set_audit_number(Obj, AuditNumber) when is_integer(AuditNumber) ->
  Obj#iot_protocol{audit_number = AuditNumber}.
