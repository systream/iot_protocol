%%%-------------------------------------------------------------------
%%% @author tihanyipeter
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Sep 2019 20:53
%%%-------------------------------------------------------------------
-author("tihanyipeter").

-define(PAYLOAD_LENGTH_SIZE, 32).
-define(AUDIT_NUMBER_SIZE, 40).
-define(PROTOCOL_VERSION_SIZE, 8).
-define(MESSAGE_TYPE_SIZE, 8).

-define(LATEST_PROTOCOL_VERSION, 1).

-record(iot_protocol, {protocol_version = ?LATEST_PROTOCOL_VERSION :: integer(),
                       message_type :: integer(),
                       audit_number = 0 :: integer(),
                       payload = <<>> :: binary(),
                       total_message_length = unknown :: integer() | unknown,
                       record_version = 1 :: pos_integer()}).

-type iot_protocol_obj() :: #iot_protocol{}.


% Message Types
-define(PING, 10).
-define(PONG, 11).