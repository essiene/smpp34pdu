-ifndef(types).
-define(types, true).

-include("smpp34pdu.hrl").

-type(generic_nack() :: #generic_nack{}).
-type(bind_receiver() :: #bind_receiver{}).
-type(bind_receiver_resp() :: #bind_receiver_resp{}).
-type(bind_transmitter() :: #bind_transmitter{}).
-type(bind_transmitter_resp() :: #bind_transmitter_resp{}).
-type(query_sm() :: #query_sm{}).
-type(query_sm_resp() :: #query_sm_resp{}).
-type(submit_sm() :: #submit_sm{}).
-type(submit_sm_resp() :: #submit_sm_resp{}).
-type(deliver_sm() :: #deliver_sm{}).
-type(deliver_sm_resp() :: #deliver_sm_resp{}).
-type(data_sm() :: #data_sm{}).
-type(data_sm_resp() :: #data_sm_resp{}).
-type(unbind() :: #unbind{}).
-type(unbind_resp() :: #unbind_resp{}).
-type(replace_sm() :: #replace_sm{}).
-type(replace_sm_resp() :: #replace_sm_resp{}).
-type(cancel_sm() :: #cancel_sm{}).
-type(cancel_sm_resp() :: #cancel_sm_resp{}).
-type(bind_transceiver() :: #bind_transceiver{}).
-type(bind_transceiver_resp() :: #bind_transceiver_resp{}).
-type(outbind() :: #outbind{}).
-type(enquire_link() :: #enquire_link{}).
-type(enquire_link_resp() :: #enquire_link_resp{}).
-type(alert_notification() :: #alert_notification{}).

-type(invalid_command_id() :: {'error', {'command_id', integer()}}).

-type(valid_pdu() ::  generic_nack()
					| bind_receiver()
					| bind_receiver_resp()
					| bind_transmitter()
					| bind_transmitter_resp()
					| query_sm()
					| query_sm_resp()
					| submit_sm()
					| submit_sm_resp()
					| deliver_sm()
					| deliver_sm_resp()
					| data_sm()
					| data_sm_resp()
					| unbind() 
					| unbind_resp()
					| replace_sm() 
					| replace_sm_resp() 
					| cancel_sm()
					| cancel_sm_resp()
					| bind_transceiver()
					| bind_transceiver_resp()
					| outbind()
					| enquire_link()
					| enquire_link_resp()
					| alert_notification()).

-type(valid_pdu_error() :: invalid_command_id()).

-type(pdu() :: #pdu{command_length :: integer(),
					command_id :: integer(),
					command_status :: integer(),
					sequence_number :: integer(), 
					body :: valid_pdu() | valid_pdu_error()}).

-endif.
