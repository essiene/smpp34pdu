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
					| bind_transceiver()
					| unbind() 
					| unbind_resp()
					| generic_nack()
					| enquire_link()
					| enquire_link_resp()
					| replace_sm_resp() 
					| cancel_sm_resp()).

-type(valid_pdu_error() :: invalid_command_id()).

-type(bind_receiver() :: #bind_receiver{}).
-type(bind_transmitter() :: #bind_transmitter{}).
-type(bind_transceiver() :: #bind_transceiver{}).
-type(unbind() :: #unbind{}).
-type(unbind_resp() :: #unbind_resp{}).
-type(generic_nack() :: #generic_nack{}).
-type(enquire_link() :: #enquire_link{}).
-type(enquire_link_resp() :: #enquire_link_resp{}).
-type(replace_sm_resp() :: #replace_sm_resp{}).
-type(cancel_sm_resp() :: #cancel_sm_resp{}).

-type(invalid_command_id() :: {'error', {'command_id', integer()}}).

-endif.
