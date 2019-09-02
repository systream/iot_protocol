-module(iot_protocol).
-include("iot_protocol.hrl").

-export([parse/1, ensemble/1]).

%%% ------------------------------
% 4 byte payload length
% 1 byte protocol version
% 1 byte message_type
% messageLength payload

-spec ensemble(iot_protocol_obj()) -> binary().
ensemble(#iot_protocol{protocol_version = ProtocolVersion, message_type = MessageType, payload = Payload} = A) ->
  ProtocolVersionBin = binary:encode_unsigned(ProtocolVersion),
  MessageTypeBin = binary:encode_unsigned(MessageType),
  PayloadSize = pad_size(byte_size(Payload)),
  <<PayloadSize/binary, ProtocolVersionBin/binary, MessageTypeBin/binary, Payload/binary>>.

-spec parse(binary()) -> iot_protocol_obj().
parse(<<PayloadLength:?PAYLOAD_LENGTH_SIZE/integer, ProtocolVersion:?PROTOCOL_VERSION_SIZE/integer, MessageType:?MESSAGE_TYPE_SIZE/integer, Rest/binary>>) ->
  #iot_protocol{
    protocol_version = ProtocolVersion,
    message_type = MessageType,
    payload = extract_payload(Rest, PayloadLength),
    total_message_length = PayloadLength+6
  }.

extract_payload(<<>>, 0) ->
  <<>>;
extract_payload(<<Message/binary>>, Size) when byte_size(Message) >= Size  ->
  <<Payload:Size/binary, _/binary>> = Message,
  binary:copy(Payload).

pad_size(Size) when Size =< ?PAYLOAD_LENGTH_SIZE ->
  <<Size:?PAYLOAD_LENGTH_SIZE/integer>>;
pad_size(_Binary) ->
  throw("Payload cannot be more thant 2^?PAYLOAD_LENGTH_SIZE").