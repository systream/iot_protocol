%%%-------------------------------------------------------------------
%%% @author Peter Tihanyi
%%% @copyright (C) 2019, systream
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-author("Peter Tihanyi").

-define(MAX_MSG_SIZE, 4294967296).

-define(PAYLOAD_LENGTH_SIZE, 32).
-define(AUDIT_NUMBER_SIZE, 40).
-define(PROTOCOL_VERSION_SIZE, 8).
-define(MESSAGE_TYPE_SIZE, 8).

-define(LATEST_PROTOCOL_VERSION, 1).

-export_type([iot_protocol_obj/0]).

-type protocol_version() :: pos_integer().
-type message_type() :: integer().
-type audit_number() :: integer().
-type payload() :: binary().
-type msg_size() :: non_neg_integer().

-record(iot_protocol, {protocol_version = ?LATEST_PROTOCOL_VERSION :: protocol_version(),
                       message_type :: message_type(),
                       audit_number = 0 :: audit_number(),
                       payload = <<>> :: payload(),
                       total_message_length = unknown :: msg_size() | unknown,
                       record_version = 1 :: pos_integer()}).

-type iot_protocol_obj() :: #iot_protocol{}.

% Message Types
-define(PING, 10).
-define(PONG, 11).

-define(LOGON, 00).

-define(REGISTER_DEVICE, 01).

-define(ACK, 02).
-define(ERROR, 03).

-define(REGISTER_IO, 5).

-define(GET, 20).
-define(ADVICE, 30).
-define(SET, 40).

-define(RESTART, 90).
-define(UPDATE_DEVICE, 91).