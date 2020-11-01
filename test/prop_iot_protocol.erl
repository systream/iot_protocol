-module(prop_iot_protocol).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../src/iot_protocol.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_assemble_is_binary() ->
    ?FORALL(MsgType, message_type(),
        begin
            {ok, Result} = iot_protocol:assemble(iot_protocol_obj:new(MsgType)),
            is_binary(Result)
        end).

prop_assemble_message_size() ->
    ?FORALL({MsgType, Payload, AuditNumber},
            {message_type(), payload(), audit_number()},
            begin
                Obj = iot_protocol_obj:new(MsgType, Payload),
                Obj1 = iot_protocol_obj:set_audit_number(Obj, AuditNumber),
                {ok, Result} = iot_protocol:assemble(Obj1),
                <<MsgSize:32/integer, Rest/binary>> = Result,
                (byte_size(Rest)+4) == MsgSize
            end).

prop_symmetric() ->
    ?FORALL({ProtocolVer, MsgType, Payload, AuditNumber},
            {protocol_ver(), message_type(), payload(), audit_number()},
            begin
                Obj = iot_protocol_obj:new(ProtocolVer, MsgType, Payload),
                Obj1 = iot_protocol_obj:set_audit_number(Obj, AuditNumber),
                case iot_protocol:assemble(Obj1) of
                    {ok, Result} ->
                        {ok, ParsedObj} = iot_protocol:parse(Result),
                        iot_protocol_obj:get_audit_number(ParsedObj) == AuditNumber andalso
                        iot_protocol_obj:get_message_type(ParsedObj) == MsgType andalso
                        iot_protocol_obj:get_payload(ParsedObj) == Payload andalso
                        iot_protocol_obj:get_protocol_version(ParsedObj) == ProtocolVer;
                    {error, unsupported_version} ->
                        true
                end
            end).

prop_reply() ->
    ?FORALL({ProtocolVer, MsgType, Payload, AuditNumber, Reply},
            {supported_protocol_versions(), message_type(), payload(), audit_number(),
             oneof([?ACK, ?ERROR])},
            begin
                Obj = iot_protocol_obj:new(ProtocolVer, MsgType, Payload),
                Obj1 = iot_protocol_obj:set_audit_number(Obj, AuditNumber),

                {ok, Result} = iot_protocol:assemble(Obj1),
                {ok, ParsedObj} = iot_protocol:parse(Result),

                CReply = iot_protocol_obj:reply(ParsedObj, Reply),
                CReply = iot_protocol_obj:reply(ParsedObj, Reply, <<>>),

                case Reply of
                    ?ACK ->
                        CReply = iot_protocol_obj:reply_ok(ParsedObj);
                    ?ERROR ->
                        CReply = iot_protocol_obj:reply_error(ParsedObj)
                end,

                {ok, ReplyResult} = iot_protocol:assemble(CReply),
                {ok, ReplyParsedObj} = iot_protocol:parse(ReplyResult),

                iot_protocol_obj:get_audit_number(ReplyParsedObj) == AuditNumber andalso
                iot_protocol_obj:get_message_type(ReplyParsedObj) == Reply
            end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

message_type() ->
    range(0, 255).

payload() ->
    binary().

audit_number() ->
    pos_integer().

supported_protocol_versions() ->
    oneof([1]).

protocol_ver() ->
    frequency([
                  {10, supported_protocol_versions()},
                  {1, range(0, 255)}
              ]).
