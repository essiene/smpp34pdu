-ifndef(types).
-define(types, true).

-include("pdu.hrl").

-type(pdu() :: #pdu{command_length :: integer(),
					command_id :: integer(),
					command_status :: integer(),
					sequence_number :: integer(), 
					body :: valid_pdu() | valid_pdu_error()}).

-type(valid_pdu() :: bind_receiver()
					| bind_transmitter()
					| bind_transceiver()).

-type(valid_pdu_error() :: invalid_command_id()).

-type(bind_receiver() :: #bind_receiver{}).
-type(bind_transmitter() :: #bind_transmitter{}).
-type(bind_transceiver() :: #bind_transceiver{}).

-type(invalid_command_id() :: {'error', {'command_id', integer()}}).

-endif.
