%%%-------------------------------------------------------------------
%%% @author tihanyipeter
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Jun 2019 16:41
%%%-------------------------------------------------------------------
-module(iot_protocol_test).
-author("tihanyipeter").

-include_lib("eunit/include/eunit.hrl").


parse_not_payload_test() ->
  assert_message(<<0, 0, 0, 0, 4, 5, 0, 0, 0, 0, 0>>, iot_protocol_obj:new(4, 5)),
  assert_message(<<0, 0, 0, 0, 1, 3, 0, 0, 0, 0, 0>>, iot_protocol_obj:new(3)).

parse_with_payload_test() ->
  assert_message(<<0, 0, 0, 5, 2, 3, 0, 0, 0, 0, 0, 1, 2, 3, 4, 5>>, iot_protocol_obj:new(2, 3, <<1, 2, 3, 4, 5>>)).

parse_with_payload_more_data_test() ->
  assert_message(<<0, 0, 0, 5, 1, 3, 0, 0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9>>,
    iot_protocol_obj:new(3, <<1, 2, 3, 4, 5>>)).

parse_with_payload_less_data_test() ->
  ?assertError(function_clause, iot_protocol:parse(<<0, 0, 0, 5, 1, 0, 0, 0, 0, 0, 3, 1, 2, 3>>)).

ensemble_empty_test() ->
  assert_ensemble(<<0, 0, 0, 0, 1, 210, 0, 0, 0, 0, 0>>, iot_protocol_obj:new(1, 210)).

ensemble_payload_test() ->
  assert_ensemble(<<0, 0, 0, 3, 1, 210, 0, 0, 0, 0, 0, 1, 2, 3>>, iot_protocol_obj:new(1, 210, <<1,2,3>>)).

ensemble_too_big_payload_payload_test() ->
  assert_ensemble(<<0, 0, 0, 3, 1, 210, 0, 0, 0, 0, 0, 1, 2, 3>>, iot_protocol_obj:new(1, 210, <<1,2,3>>)).

assert_ensemble(Binary, Message) ->
  ?assertEqual(Binary, iot_protocol:ensemble(Message)).

assert_message(Message, ParsedMessage) ->
  ?assertEqual(ParsedMessage, iot_protocol:parse(Message)).