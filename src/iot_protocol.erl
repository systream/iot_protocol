-module(iot_protocol).
-include("iot_protocol.hrl").

-export([parse/1, ensemble/1]).

%%% ------------------------------
% 4 byte payload length
% 1 byte protocol version
% 1 byte message_type
% 5 byte audit_number
% messageLength payload

-spec ensemble(iot_protocol_obj()) -> binary().
ensemble(#iot_protocol{protocol_version = ProtocolVersion,
                        message_type = MessageType,
                        payload = Payload,
                        audit_number = AuditNumber}) ->
  ProtocolVersionBin = binary:encode_unsigned(ProtocolVersion),
  MessageTypeBin = binary:encode_unsigned(MessageType),
  PayloadSize = get_payload_size(Payload),
  <<PayloadSize/binary, ProtocolVersionBin/binary, MessageTypeBin/binary,
    AuditNumber:?AUDIT_NUMBER_SIZE/integer, Payload/binary>>.

-spec parse(binary()) -> iot_protocol_obj().
parse(<<PayloadLength:?PAYLOAD_LENGTH_SIZE/integer,
        ProtocolVersion:?PROTOCOL_VERSION_SIZE/integer,
        MessageType:?MESSAGE_TYPE_SIZE/integer,
        AuditNumber:?AUDIT_NUMBER_SIZE/integer,
        Rest/binary>>) ->
  #iot_protocol{
    protocol_version = ProtocolVersion,
    message_type = MessageType,
    audit_number = AuditNumber,
    payload = extract_payload(Rest, PayloadLength),
    total_message_length = PayloadLength+11
  }.

extract_payload(<<>>, 0) ->
  <<>>;
extract_payload(<<Message/binary>>, Size) when byte_size(Message) >= Size  ->
  <<Payload:Size/binary, _/binary>> = Message,
  binary:copy(Payload).

get_payload_size(<<Payload/binary>>) ->
  pad_size(byte_size(Payload)).

pad_size(Size) when Size =< ?PAYLOAD_LENGTH_SIZE ->
  <<Size:?PAYLOAD_LENGTH_SIZE/integer>>;
pad_size(_Binary) ->
  throw("Payload cannot be more thant 2^?PAYLOAD_LENGTH_SIZE").