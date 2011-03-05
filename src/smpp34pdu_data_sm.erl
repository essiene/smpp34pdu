-module(smpp34pdu_data_sm).
-include("smpp34pdu.hrl").
-include("types.hrl").
-include("tlv_macros.hrl").

-export([pack/1, unpack/1]).
-import(pdu_data, [cstring_to_bin/2, string_to_bin/2, integer_to_bin/2]).
-import(pdu_data, [bin_to_cstring/2, bin_to_string/2, bin_to_integer/2]).

-spec(pack/1 :: (data_sm()) -> binary()).
-spec(unpack/1 :: (binary()) -> data_sm()).

pack(#data_sm{service_type=SrvType,
		source_addr_ton=SrcAddrTon,
		source_addr_npi=SrcAddrNpi,
		source_addr=SrcAddr,
		dest_addr_ton=DestAddrTon,
		dest_addr_npi=DestAddrNpi,
		destination_addr=DestAddr,
		esm_class=EsmClass,
		registered_delivery=RgdDelivery,
		data_coding=DataCoding,
		source_port=SrcPort,
		source_addr_subunit=SrcAddrSubUnit,
		source_network_type=SrcNetworkType,
		source_bearer_type=SrcBearerType,
		source_telematics_id=SrcTelematicsId,
		destination_port=DestPort,
		dest_addr_subunit=DestAddrSubUnit,
		dest_network_type=DestNetworkType,
		dest_bearer_type=DestBearerType,
		dest_telematics_id=DestTelematicsId,
		sar_msg_ref_num=SarMsgRefNum,
		sar_total_segments=SarTotalSegments,
		sar_segment_seqnum=SarSegSeqnum,
		more_messages_to_send=MoreMsgToSend,
		qos_time_to_live=QosTTL,
		payload_type=PayloadType,
		message_payload=MsgPayload,
		set_dpf=SetDpf,
		receipted_message_id=RcptedMsgId,
		message_state=MsgState,
		network_error_code=NetworkErrorCode,
		user_message_reference=UsrMsgRef,
		privacy_indicator=PrivacyIndicator,
		callback_num=CallbackNum,
		callback_num_pres_ind=CallbackNumPresInd,
		callback_num_atag=CallbackNumAtag,
		source_subaddress=SrcSubaddress,
		dest_subaddress=DestSubaddress,
		user_response_code=UsrResponseCode,
		display_time=DisplayTime,
		sms_signal=SmsSignal,
		ms_validity=MsValidity,
		ms_msg_wait_facilities=MsMsgWaitFacilities,
		number_of_messages=NumberOfMessages,
		alert_on_message_delivery=AlertOnMsgDelivery,
		language_indicator=LanguageIndicator,
		its_reply_type=ItsReplyType,
		its_session_info=ItsSessionInfo}) ->

		L = [cstring_to_bin(SrvType, 6),
					integer_to_bin(SrcAddrTon, 1),
					integer_to_bin(SrcAddrNpi, 1),
					cstring_to_bin(SrcAddr, 65),
					integer_to_bin(DestAddrTon, 1),
					integer_to_bin(DestAddrNpi, 1),
					cstring_to_bin(DestAddr, 65),
					integer_to_bin(EsmClass, 1),
					integer_to_bin(RgdDelivery, 1),
					integer_to_bin(DataCoding, 1),
					tlv:pack(?SOURCE_PORT, SrcPort),
					tlv:pack(?SOURCE_ADDR_SUBUNIT, SrcAddrSubUnit),
					tlv:pack(?SOURCE_NETWORK_TYPE, SrcNetworkType),
					tlv:pack(?SOURCE_BEARER_TYPE, SrcBearerType),
					tlv:pack(?SOURCE_TELEMATICS_ID, SrcTelematicsId),
					tlv:pack(?DESTINATION_PORT, DestPort),
					tlv:pack(?DEST_ADDR_SUBUNIT, DestAddrSubUnit),
					tlv:pack(?DEST_NETWORK_TYPE, DestNetworkType),
					tlv:pack(?DEST_BEARER_TYPE, DestBearerType),
					tlv:pack(?DEST_TELEMATICS_ID, DestTelematicsId),
					tlv:pack(?SAR_MSG_REF_NUM, SarMsgRefNum),
					tlv:pack(?SAR_TOTAL_SEGMENTS, SarTotalSegments),
					tlv:pack(?SAR_SEGMENT_SEQNUM, SarSegSeqnum),
					tlv:pack(?MORE_MESSAGES_TO_SEND, MoreMsgToSend),
					tlv:pack(?QOS_TIME_TO_LIVE, QosTTL),
					tlv:pack(?PAYLOAD_TYPE, PayloadType),
					tlv:pack(?MESSAGE_PAYLOAD, MsgPayload),
					tlv:pack(?SET_DPF, SetDpf),
					tlv:pack(?RECEIPTED_MESSAGE_ID, RcptedMsgId),
					tlv:pack(?MESSAGE_STATE, MsgState),
					tlv:pack(?NETWORK_ERROR_CODE, NetworkErrorCode),
					tlv:pack(?USER_MESSAGE_REFERENCE, UsrMsgRef),
					tlv:pack(?PRIVACY_INDICATOR, PrivacyIndicator),
					tlv:pack(?CALLBACK_NUM, CallbackNum),
					tlv:pack(?CALLBACK_NUM_PRES_IND, CallbackNumPresInd),
					tlv:pack(?CALLBACK_NUM_ATAG, CallbackNumAtag),
					tlv:pack(?SOURCE_SUBADDRESS, SrcSubaddress),
					tlv:pack(?DEST_SUBADDRESS, DestSubaddress),
					tlv:pack(?USER_RESPONSE_CODE, UsrResponseCode),
					tlv:pack(?DISPLAY_TIME, DisplayTime),
					tlv:pack(?SMS_SIGNAL, SmsSignal),
					tlv:pack(?MS_VALIDITY, MsValidity),
					tlv:pack(?MS_MSG_WAIT_FACILITIES, MsMsgWaitFacilities),
					tlv:pack(?NUMBER_OF_MESSAGES, NumberOfMessages),
					tlv:pack(?ALERT_ON_MESSAGE_DELIVERY, AlertOnMsgDelivery),
					tlv:pack(?LANGUAGE_INDICATOR, LanguageIndicator),
					tlv:pack(?ITS_REPLY_TYPE, ItsReplyType),
					tlv:pack(?ITS_SESSION_INFO, ItsSessionInfo)],

		list_to_binary(L).

unpack(Bin0) ->
	{SrvType, Bin1} = bin_to_cstring(Bin0, 6),
	{SrcAddrTon, Bin2} = bin_to_integer(Bin1, 1),
	{SrcAddrNpi, Bin3} = bin_to_integer(Bin2, 1),
	{SrcAddr, Bin4} = bin_to_cstring(Bin3, 65),
	{DestAddrTon, Bin5} = bin_to_integer(Bin4, 1),
	{DestAddrNpi, Bin6} = bin_to_integer(Bin5, 1),
	{DestAddr, Bin7} = bin_to_cstring(Bin6, 65),
	{EsmClass, Bin8} = bin_to_integer(Bin7, 1),
	{RgdDelivery, Bin9} = bin_to_integer(Bin8, 1),
	{DataCoding, Bin10} = bin_to_integer(Bin9, 1),

	unpack_tlv_fields(Bin10, #data_sm{service_type=SrvType,
		source_addr_ton=SrcAddrTon,
		source_addr_npi=SrcAddrNpi,
		source_addr=SrcAddr,
		dest_addr_ton=DestAddrTon,
		dest_addr_npi=DestAddrNpi,
		destination_addr=DestAddr,
		esm_class=EsmClass,
		registered_delivery=RgdDelivery,
		data_coding=DataCoding}).

?TLV_UNPACK_EMPTY_BIN();
?TLV_UNPACK_FIELD(data_sm, source_port, ?SOURCE_PORT);
?TLV_UNPACK_FIELD(data_sm, source_addr_subunit, ?SOURCE_ADDR_SUBUNIT);
?TLV_UNPACK_FIELD(data_sm, source_network_type, ?SOURCE_NETWORK_TYPE);
?TLV_UNPACK_FIELD(data_sm, source_bearer_type, ?SOURCE_BEARER_TYPE);
?TLV_UNPACK_FIELD(data_sm, source_telematics_id, ?SOURCE_TELEMATICS_ID);
?TLV_UNPACK_FIELD(data_sm, destination_port, ?DESTINATION_PORT);
?TLV_UNPACK_FIELD(data_sm, dest_addr_subunit, ?DEST_ADDR_SUBUNIT);
?TLV_UNPACK_FIELD(data_sm, dest_network_type, ?DEST_NETWORK_TYPE);
?TLV_UNPACK_FIELD(data_sm, dest_bearer_type, ?DEST_BEARER_TYPE);
?TLV_UNPACK_FIELD(data_sm, dest_telematics_id, ?DEST_TELEMATICS_ID);
?TLV_UNPACK_FIELD(data_sm, sar_msg_ref_num, ?SAR_MSG_REF_NUM);
?TLV_UNPACK_FIELD(data_sm, sar_total_segments, ?SAR_TOTAL_SEGMENTS);
?TLV_UNPACK_FIELD(data_sm, sar_segment_seqnum, ?SAR_SEGMENT_SEQNUM);
?TLV_UNPACK_FIELD(data_sm, more_messages_to_send, ?MORE_MESSAGES_TO_SEND);
?TLV_UNPACK_FIELD(data_sm, qos_time_to_live, ?QOS_TIME_TO_LIVE);
?TLV_UNPACK_FIELD(data_sm, payload_type, ?PAYLOAD_TYPE);
?TLV_UNPACK_FIELD(data_sm, message_payload, ?MESSAGE_PAYLOAD);
?TLV_UNPACK_FIELD(data_sm, set_dpf, ?SET_DPF);
?TLV_UNPACK_FIELD(data_sm, receipted_message_id, ?RECEIPTED_MESSAGE_ID);
?TLV_UNPACK_FIELD(data_sm, message_state, ?MESSAGE_STATE);
?TLV_UNPACK_FIELD(data_sm, network_error_code, ?NETWORK_ERROR_CODE);
?TLV_UNPACK_FIELD(data_sm, user_message_reference, ?USER_MESSAGE_REFERENCE);
?TLV_UNPACK_FIELD(data_sm, privacy_indicator, ?PRIVACY_INDICATOR);
?TLV_UNPACK_FIELD(data_sm, callback_num, ?CALLBACK_NUM);
?TLV_UNPACK_FIELD(data_sm, callback_num_pres_ind, ?CALLBACK_NUM_PRES_IND);
?TLV_UNPACK_FIELD(data_sm, callback_num_atag, ?CALLBACK_NUM_ATAG);
?TLV_UNPACK_FIELD(data_sm, source_subaddress, ?SOURCE_SUBADDRESS);
?TLV_UNPACK_FIELD(data_sm, dest_subaddress, ?DEST_SUBADDRESS);
?TLV_UNPACK_FIELD(data_sm, user_response_code, ?USER_RESPONSE_CODE);
?TLV_UNPACK_FIELD(data_sm, display_time, ?DISPLAY_TIME);
?TLV_UNPACK_FIELD(data_sm, sms_signal, ?SMS_SIGNAL);
?TLV_UNPACK_FIELD(data_sm, ms_validity, ?MS_VALIDITY);
?TLV_UNPACK_FIELD(data_sm, ms_msg_wait_facilities, ?MS_MSG_WAIT_FACILITIES);
?TLV_UNPACK_FIELD(data_sm, number_of_messages, ?NUMBER_OF_MESSAGES);
?TLV_UNPACK_FIELD(data_sm, alert_on_message_delivery, ?ALERT_ON_MESSAGE_DELIVERY);
?TLV_UNPACK_FIELD(data_sm, language_indicator, ?LANGUAGE_INDICATOR);
?TLV_UNPACK_FIELD(data_sm, its_reply_type, ?ITS_REPLY_TYPE);
?TLV_UNPACK_FIELD(data_sm, its_session_info, ?ITS_SESSION_INFO);
?TLV_UNPACK_UNEXPECTED().
