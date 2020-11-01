-module(iot_protocol_v1).
-author("Peter Tihanyi").

-include("iot_protocol.hrl").

-behavior(iot_protocol).

-define(HEADER_SIZE, 11).

%% API
-export([parse/1, assemble/1]).

-spec assemble(iot_protocol_obj()) -> binary().
assemble(#iot_protocol{protocol_version = ProtocolVersion,
                       message_type = MessageType,
                       payload = Payload,
                       audit_number = AuditNumber}) ->
  MessageSize = get_message_size(Payload),
  <<MessageSize/binary, ProtocolVersion:8/integer, MessageType:8/integer,
    AuditNumber:?AUDIT_NUMBER_SIZE/integer, Payload/binary>>.

%% @doc Parse binary message to iot protocol object.
-spec parse(binary()) -> {ok, iot_protocol_obj()} | chunked_message.
parse(<<MessageLength:?PAYLOAD_LENGTH_SIZE/integer,
        ProtocolVersion:?PROTOCOL_VERSION_SIZE/integer,
        MessageType:?MESSAGE_TYPE_SIZE/integer,
        AuditNumber:?AUDIT_NUMBER_SIZE/integer,
        Rest/binary>>) ->
  PayloadLength = MessageLength - ?HEADER_SIZE,
  case extract_payload(Rest, PayloadLength) of
    chunked_message ->
      chunked_message;
    Payload ->
      {ok,
       #iot_protocol{
        protocol_version = ProtocolVersion,
        message_type = MessageType,
        audit_number = AuditNumber,
        payload = Payload,
        total_message_length = MessageLength
      }}
  end;
parse(_) ->
  chunked_message.

-spec extract_payload(binary(), msg_size()) -> payload() | chunked_message.
extract_payload(<<>>, 0) ->
  <<>>;
extract_payload(<<Message/binary>>, Size) when byte_size(Message) >= Size ->
  <<Payload:Size/binary, _/binary>> = Message,
  binary:copy(Payload);
extract_payload(_, _) ->
  chunked_message.

-spec get_message_size(payload()) -> binary().
get_message_size(<<Payload/binary>>) ->
  pad_size(byte_size(Payload) + 11).

-spec pad_size(msg_size()) -> binary().
pad_size(Size) ->
  <<Size:?PAYLOAD_LENGTH_SIZE/integer>>.