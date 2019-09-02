%%%-------------------------------------------------------------------
%%% @author tihanyipeter
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Jun 2019 16:41
%%%-------------------------------------------------------------------
-module(iot_protocol_obj_test).
-author("tihanyipeter").

-include_lib("eunit/include/eunit.hrl").

just_type_test() ->
  Obj = iot_protocol_obj:new(1),
  ?assertEqual(
    1,
    iot_protocol_obj:get_message_type(Obj)
  ),
  ?assertEqual(
    <<>>,
    iot_protocol_obj:get_payload(Obj)
  ),
  ?assertEqual(
    11,
    iot_protocol_obj:get_total_message_length(Obj)
  ).

type_and_version_test() ->
  Obj = iot_protocol_obj:new(2, 3),
  ?assertEqual(
    3,
    iot_protocol_obj:get_message_type(Obj)
  ),
  ?assertEqual(
    2,
    iot_protocol_obj:get_protocol_version(Obj)
  ),
  ?assertEqual(
    <<>>,
    iot_protocol_obj:get_payload(Obj)
  ),
  ?assertEqual(
    11,
    iot_protocol_obj:get_total_message_length(Obj)
  ).

type_and_payload_test() ->
  Obj = iot_protocol_obj:new(4, <<10, 20>>),
  ?assertEqual(
    4,
    iot_protocol_obj:get_message_type(Obj)
  ),
  ?assertEqual(
    1,
    iot_protocol_obj:get_protocol_version(Obj)
  ),
  ?assertEqual(
    <<10, 20>>,
    iot_protocol_obj:get_payload(Obj)
  ),
  ?assertEqual(
    13,
    iot_protocol_obj:get_total_message_length(Obj)
  ).


type_and_protocol_and_payload_test() ->
  Obj = iot_protocol_obj:new(10, 7, <<10, 20, 30>>),
  ?assertEqual(
    7,
    iot_protocol_obj:get_message_type(Obj)
  ),
  ?assertEqual(
    10,
    iot_protocol_obj:get_protocol_version(Obj)
  ),
  ?assertEqual(
    <<10, 20, 30>>,
    iot_protocol_obj:get_payload(Obj)
  ),
  ?assertEqual(
    14,
    iot_protocol_obj:get_total_message_length(Obj)
  ).

audit_number_test() ->
  Obj = iot_protocol_obj:new(5),
  NewObj = iot_protocol_obj:set_audit_number(Obj, 190802250101010),
  ?assertEqual(190802250101010, iot_protocol_obj:get_audit_number(NewObj)).