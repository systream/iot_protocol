%%%-------------------------------------------------------------------
%%% @author Peter Tihanyi
%%% @copyright (C) 2020, Systream Ltd
%%% @doc
%%%
%%% @end
%%% Created : 05. Sep 2020 19:20
%%%-------------------------------------------------------------------
-module(prop_iot_protocol).
-author("Peter Tihanyi").

-include_lib("proper/include/proper.hrl").

prop_iot_compile() ->
  ?FORALL({ProtocolVersion, MessageType, Payload},
          {protocol_version(), message_type(), binary()},
          begin
            Obj = iot_protocol_obj:new(ProtocolVersion, MessageType, Payload),
            Binary = iot_protocol:assemble(Obj),
            DecodedObj = iot_protocol:parse(Binary),
            Obj == DecodedObj andalso
            iot_protocol_obj:get_payload(DecodedObj) == Payload andalso
            iot_protocol_obj:get_message_type(DecodedObj) == MessageType andalso
            iot_protocol_obj:get_protocol_version(DecodedObj) == ProtocolVersion
          end).

prop_iot_reply() ->
  ?FORALL({ProtocolVersion, MsgType, Payload, ReplyMsgType, ReplyPayload},
          {protocol_version(), message_type(), binary(), message_type(), binary()},
          begin
            Obj = iot_protocol_obj:new(ProtocolVersion, MsgType, Payload),
            Reply = iot_protocol_obj:reply(Obj, ReplyMsgType, ReplyPayload),
            iot_protocol_obj:get_audit_number(Obj) == iot_protocol_obj:get_audit_number(Reply) andalso
            iot_protocol_obj:get_payload(Reply) == ReplyPayload andalso
            iot_protocol_obj:get_message_type(Reply) == ReplyMsgType
          end).


protocol_version() ->
  choose(0, 255).

message_type() ->
  choose(0, 255).