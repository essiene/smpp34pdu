-ifndef(pdu).
-define(pdu, true).

-include("../src/constants.hrl").

-record(pdu, {command_length, 
        command_id, 
        command_status,
        sequence_number, body}).

-record(bind_transmitter, {system_id=?DEFAULT_CSTRING,
        password=?DEFAULT_CSTRING,
        system_type=?DEFAULT_CSTRING,
        interface_version=?VERSION,
        addr_ton=?DEFAULT_TON,
        addr_npi=?DEFAULT_NPI,
        address_range=?DEFAULT_CSTRING}).

-record(bind_transmitter_resp, {system_id=?DEFAULT_CSTRING, optional}).

-record(bind_receiver, {system_id=?DEFAULT_CSTRING,
        password=?DEFAULT_CSTRING,
        system_type=?DEFAULT_CSTRING,
        interface_version=?VERSION,
        addr_ton=?DEFAULT_TON,
        addr_npi=?DEFAULT_NPI,
        address_range=?DEFAULT_CSTRING}).

-record(bind_receiver_resp, {system_id=?DEFAULT_CSTRING, optional}).

-record(bind_transceiver, {system_id=?DEFAULT_CSTRING,
        password=?DEFAULT_CSTRING,
        system_type=?DEFAULT_CSTRING,
        interface_version=?VERSION,
        addr_ton=?DEFAULT_TON,
        addr_npi=?DEFAULT_NPI,
        address_range=?DEFAULT_CSTRING}).

-record(bind_transceiver_resp, {system_id=?DEFAULT_CSTRING, optional}).

-record(outbind, {system_id=?DEFAULT_STRING,
        password=?DEFAULT_STRING}).

-record(generic_nack, {}).

-record(submit_sm, {service_type=?DEFAULT_CSTRING,
        source_addr_ton=?DEFAULT_TON,
        source_addr_npi=?DEFAULT_NPI,
        source_addr=?DEFAULT_CSTRING,
        dest_addr_ton = ?DEFAULT_TON,
        dest_addr_npi = ?DEFAULT_NPI,
        destination_addr=?DEFAULT_CSTRING,
        esm_class = 0,
        protocol_id = 0,
        priority_flag = 1,
        schedule_delivery_time=?DEFAULT_CSTRING,
        validity_period=?DEFAULT_CSTRING,
        registered_deliver = 0,
        replace_if_present = 0,
        data_coding = 0,
        sm_default_msg_id = 0,
        sm_length=0,
        short_message=?DEFAULT_STRING
        }).

-record(submit_sm_resp, {message_id=?DEFAULT_CSTRING}).

%-record(submit_multi).
%-record(submit_multi_resp).

-record(deliver_sm, {service_type=?DEFAULT_CSTRING,
        source_addr_ton=?DEFAULT_TON,
        source_addr_npi=?DEFAULT_NPI,
        source_addr=?DEFAULT_CSTRING,
        dest_addr_ton = ?DEFAULT_TON,
        dest_addr_npi = ?DEFAULT_NPI,
        destination_addr=?DEFAULT_CSTRING,
        esm_class = 0,
        protocol_id = 0,
        priority_flag = 1,
        schedule_delivery_time=?DEFAULT_CSTRING,
        validity_period=?DEFAULT_CSTRING,
        registered_deliver = 0,
        replace_if_present = 0,
        data_coding = 0,
        sm_default_msg_id = 0,
        sm_length=0,
        short_message=?DEFAULT_STRING
        }).

-record(deliver_sm_resp, {message_id=?DEFAULT_CSTRING}).

-record(query_sm, {message_id=?DEFAULT_CSTRING,
        source_addr_ton=?DEFAULT_TON,
        source_addr_npi=?DEFAULT_NPI,
        source_addr=?DEFAULT_CSTRING}).

-record(query_sm_resp, {message_id=?DEFAULT_CSTRING,
        final_date=?DEFAULT_CSTRING,
        message_state=0,
        error_code=0}).

-record(cancel_sm, {service_type=?DEFAULT_CSTRING,
        message_id=?DEFAULT_CSTRING,
        source_addr_ton=?DEFAULT_TON,
        source_addr_npi=?DEFAULT_NPI,
        source_addr=?DEFAULT_CSTRING,
        dest_addr_ton=?DEFAULT_TON,
        dest_addr_npi=?DEFAULT_NPI,
        destination_addr=?DEFAULT_CSTRING}).

-record(cancel_sm_resp, {}).

-record(replace_sm, {message_id=?DEFAULT_CSTRING,
        source_addr_ton=?DEFAULT_TON,
        source_addr_npi=?DEFAULT_NPI,
        source_addr=?DEFAULT_CSTRING,
        schedule_delivery_time=?DEFAULT_CSTRING,
        validity_period=?DEFAULT_CSTRING,
        registered_deliver = 0,
        sm_default_msg_id = 0,
        sm_length=0,
        short_message=?DEFAULT_STRING}).

-record(replace_sm_resp, {}).


-record(enquire_link, {}).
-record(enquire_link_resp, {}).
-record(unbind, {}).
-record(unbind_resp, {}).

-endif.
