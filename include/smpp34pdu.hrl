-ifndef(smpp34pdu).
-define(smpp34pdu, true).

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

-record(bind_transmitter_resp, {system_id=?DEFAULT_CSTRING, 
		sc_interface_version}).

-record(bind_receiver, {system_id=?DEFAULT_CSTRING,
        password=?DEFAULT_CSTRING,
        system_type=?DEFAULT_CSTRING,
        interface_version=?VERSION,
        addr_ton=?DEFAULT_TON,
        addr_npi=?DEFAULT_NPI,
        address_range=?DEFAULT_CSTRING}).

-record(bind_receiver_resp, {system_id=?DEFAULT_CSTRING, 
		sc_interface_version}).

-record(bind_transceiver, {system_id=?DEFAULT_CSTRING,
        password=?DEFAULT_CSTRING,
        system_type=?DEFAULT_CSTRING,
        interface_version=?VERSION,
        addr_ton=?DEFAULT_TON,
        addr_npi=?DEFAULT_NPI,
        address_range=?DEFAULT_CSTRING}).

-record(bind_transceiver_resp, {system_id=?DEFAULT_CSTRING, 
		sc_interface_version}).

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
        registered_delivery = 0,
        replace_if_present_flag = 0,
        data_coding = 0,
        sm_default_msg_id = 0,
        sm_length=0,
        short_message=?DEFAULT_STRING, 
		user_message_reference, 
		source_port,
		source_addr_subunit,
		destination_port,
		dest_addr_subunit,
		sar_msg_ref_num,
		sar_total_segments,
		sar_segment_seqnum,
		more_messages_to_send,
		payload_type,
		message_payload,
		privacy_indicator,
		callback_num,
		callback_num_pres_ind,
		callback_num_atag,
		source_subaddress,
		dest_subaddress,
		user_response_code,
		display_time,
		sms_signal,
		ms_validity,
		ms_msg_wait_facilities,
		number_of_messages,
		alert_on_message_delivery,
		language_indicator,
		its_reply_type,
		its_session_info,
		ussd_service_op
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
        registered_delivery = 0,
        replace_if_present_flag = 0,
        data_coding = 0,
        sm_default_msg_id = 0,
        sm_length=0,
        short_message=?DEFAULT_STRING,
		user_message_reference,	
		source_port,
		destination_port,
		sar_msg_ref_num,
		sar_total_segments,
		sar_segment_seqnum,
		user_response_code,
		privacy_indicator,
		payload_type,
		message_payload,
		callback_num,
		source_subaddress,
		dest_subaddress,
		language_indicator,
		its_session_info,
		network_error_code,
		message_state,
		receipted_message_id
        }).

-record(deliver_sm_resp, {message_id=?DEFAULT_CSTRING}).

-record(query_sm, {message_id=?DEFAULT_CSTRING,
        source_addr_ton=?DEFAULT_TON,
        source_addr_npi=?DEFAULT_NPI,
        source_addr=?DEFAULT_CSTRING}).

-record(data_sm_resp, {message_id=?DEFAULT_CSTRING,
		delivery_failure_reason,
		network_error_code,
		additional_status_info_text,
		dpf_result}).

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
        registered_delivery = 0,
        sm_default_msg_id = 0,
        sm_length=0,
        short_message=?DEFAULT_STRING}).

-record(replace_sm_resp, {}).


-record(enquire_link, {}).
-record(enquire_link_resp, {}).
-record(unbind, {}).
-record(unbind_resp, {}).

-record(alert_notification, {source_addr_ton=?DEFAULT_TON,
		source_addr_npi=?DEFAULT_NPI,
		source_addr=?DEFAULT_CSTRING,
		esme_addr_ton=?DEFAULT_TON,
		esme_addr_npi=?DEFAULT_NPI,
		esme_addr=?DEFAULT_CSTRING,
		ms_availability_status}).
-endif.
