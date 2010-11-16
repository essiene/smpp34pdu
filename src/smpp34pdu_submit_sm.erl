-module(smpp34pdu_submit_sm).
-include("smpp34pdu.hrl").
-include("types.hrl").
-include("tlv_macros.hrl").

-export([pack/1,unpack/1]).
-import(pdu_data, [cstring_to_bin/2, string_to_bin/2, integer_to_bin/2]).
-import(pdu_data, [bin_to_cstring/2, bin_to_string/2, bin_to_integer/2]).

-spec(pack/1 :: (submit_sm()) -> binary()).
-spec(unpack/1 :: (binary()) -> submit_sm()).

pack(#submit_sm{service_type=SrvType,
		source_addr_ton=SrcAddrTon,
		source_addr_npi=SrcAddrNpi,
		source_addr=SrcAddr,
		dest_addr_ton=DestAddrTon,
		dest_addr_npi=DestAddrNpi,
		destination_addr=DestAddr,
		esm_class=EsmClass,
		protocol_id=ProtoId,
		priority_flag=PrioFlag,
		schedule_delivery_time=SchDlvTime,
		validity_period=ValPeriod,
		registered_delivery=RgdDelivery,
		replace_if_present_flag=RplcIfPresentFlg,
		data_coding=DataCoding,
		sm_default_msg_id=SmDfltMsgId,
		short_message=ShortMessage, 
		user_message_reference=UsrMsgRef, 
		source_port=SrcPort, 
		source_addr_subunit=SrcAddrSubUnit, 
		destination_port=DstPort, 
		dest_addr_subunit=DstAddrSubUnit, 
		sar_msg_ref_num=SarMsgRefNum, 
		sar_total_segments=SarTotalSegments, 
		sar_segment_seqnum=SarSegSeqnum, 
		more_messages_to_send=MoreMsgToSend, 
		payload_type=PayloadType,
		message_payload=MessagePayload,
		privacy_indicator=PrivacyIndicator,
		callback_num=CallbackNum,
		callback_num_pres_ind=CallbackNumPresInd,
		callback_num_atag=CallbackNumAtag,
		source_subaddress=SourceSubaddress,
		dest_subaddress=DestSubaddress,
		user_response_code=UserResponseCode,
		display_time=DisplayTime,
		sms_signal=SmsSignal,
		ms_validity=MsValidity,
		ms_msg_wait_facilities=MsMsgWaitFacilities,
		number_of_messages=NumberOfMessages,
		alert_on_message_delivery=AlertOnMsgDelivery,
		language_indicator=LanguageIndicator,
		its_reply_type=ItsReplyType,
		its_session_info=ItsSessionInfo,
		ussd_service_op=UssdServiceOp}) ->

		SmLen = length(ShortMessage),


		L = [cstring_to_bin(SrvType,6),
					   integer_to_bin(SrcAddrTon, 1),
					   integer_to_bin(SrcAddrNpi, 1),
					   cstring_to_bin(SrcAddr, 21),
					   integer_to_bin(DestAddrTon, 1),
					   integer_to_bin(DestAddrNpi, 1),
					   cstring_to_bin(DestAddr, 21),
					   integer_to_bin(EsmClass, 1),
					   integer_to_bin(ProtoId, 1),
					   integer_to_bin(PrioFlag, 1),
					   cstring_to_bin(SchDlvTime, 17),
					   cstring_to_bin(ValPeriod, 17),
					   integer_to_bin(RgdDelivery, 1),
					   integer_to_bin(RplcIfPresentFlg, 1),
					   integer_to_bin(DataCoding, 1),
					   integer_to_bin(SmDfltMsgId, 1),
					   integer_to_bin(SmLen, 1),
					   string_to_bin(ShortMessage, SmLen),
					   tlv:pack(?USER_MESSAGE_REFERENCE, UsrMsgRef), 
					   tlv:pack(?SOURCE_PORT, SrcPort), 
					   tlv:pack(?SOURCE_ADDR_SUBUNIT, SrcAddrSubUnit), 
					   tlv:pack(?DESTINATION_PORT, DstPort), 
					   tlv:pack(?DEST_ADDR_SUBUNIT,DstAddrSubUnit), 
					   tlv:pack(?SAR_MSG_REF_NUM, SarMsgRefNum), 
					   tlv:pack(?SAR_TOTAL_SEGMENTS, SarTotalSegments), 
					   tlv:pack(?SAR_SEGMENT_SEQNUM, SarSegSeqnum), 
					   tlv:pack(?MORE_MESSAGES_TO_SEND, MoreMsgToSend), 
					   tlv:pack(?PAYLOAD_TYPE, PayloadType),
					   tlv:pack(?MESSAGE_PAYLOAD, MessagePayload),
					   tlv:pack(?PRIVACY_INDICATOR, PrivacyIndicator),
					   tlv:pack_multi(?CALLBACK_NUM, CallbackNum),
					   tlv:pack_multi(?CALLBACK_NUM_PRES_IND, CallbackNumPresInd),
					   tlv:pack_multi(?CALLBACK_NUM_ATAG, CallbackNumAtag),
					   tlv:pack(?SOURCE_SUBADDRESS, SourceSubaddress),
					   tlv:pack(?DEST_SUBADDRESS, DestSubaddress),
					   tlv:pack(?USER_RESPONSE_CODE, UserResponseCode),
					   tlv:pack(?DISPLAY_TIME, DisplayTime),
					   tlv:pack(?SMS_SIGNAL, SmsSignal),
					   tlv:pack(?MS_VALIDITY, MsValidity),
					   tlv:pack(?MS_MSG_WAIT_FACILITIES, MsMsgWaitFacilities),
					   tlv:pack(?NUMBER_OF_MESSAGES, NumberOfMessages),
					   tlv:pack(?ALERT_ON_MESSAGE_DELIVERY, AlertOnMsgDelivery),
					   tlv:pack(?LANGUAGE_INDICATOR, LanguageIndicator),
					   tlv:pack(?ITS_REPLY_TYPE, ItsReplyType),
					   tlv:pack(?ITS_SESSION_INFO, ItsSessionInfo),
					   tlv:pack(?USSD_SERVICE_OP, UssdServiceOp)],

		list_to_binary(L).


unpack(Bin0) ->
	{SrvType, Bin1} = bin_to_cstring(Bin0, 6),
	{SrcAddrTon, Bin2} = bin_to_integer(Bin1, 1),
	{SrcAddrNpi, Bin3} = bin_to_integer(Bin2, 1),
	{SrcAddr, Bin4} = bin_to_cstring(Bin3, 21),
	{DestAddrTon, Bin5} = bin_to_integer(Bin4, 1),
	{DestAddrNpi, Bin6} = bin_to_integer(Bin5, 1),
	{DestAddr, Bin7} = bin_to_cstring(Bin6, 21),
	{EsmClass, Bin8} = bin_to_integer(Bin7, 1),
	{ProtoId, Bin9} = bin_to_integer(Bin8, 1),
	{PrioFlag, Bin10} = bin_to_integer(Bin9, 1),
	{SchDlvTime, Bin11} = bin_to_cstring(Bin10, 17),
	{ValPeriod, Bin12} = bin_to_cstring(Bin11, 17),
	{RgdDelivery, Bin13} = bin_to_integer(Bin12, 1),
	{RplcIfPresentFlg, Bin14} = bin_to_integer(Bin13, 1),
	{DataCoding, Bin15} = bin_to_integer(Bin14, 1),
	{SmDfltMsgId, Bin16} = bin_to_integer(Bin15, 1),
	{SmLen, Bin17} = bin_to_integer(Bin16, 1),
	{ShortMessage, Bin18} = bin_to_string(Bin17, SmLen),

	unpack_tlv_fields(Bin18, #submit_sm{service_type=SrvType,
		source_addr_ton=SrcAddrTon,
		source_addr_npi=SrcAddrNpi,
		source_addr=SrcAddr,
		dest_addr_ton=DestAddrTon,
		dest_addr_npi=DestAddrNpi,
		destination_addr=DestAddr,
		esm_class=EsmClass,
		protocol_id=ProtoId,
		priority_flag=PrioFlag,
		schedule_delivery_time=SchDlvTime,
		validity_period=ValPeriod,
		registered_delivery=RgdDelivery,
		replace_if_present_flag=RplcIfPresentFlg,
		data_coding=DataCoding,
		sm_default_msg_id=SmDfltMsgId,
		sm_length=SmLen,
		short_message=ShortMessage}).	

?TLV_UNPACK_EMPTY_BIN();
?TLV_UNPACK_FIELD(submit_sm, user_message_reference, ?USER_MESSAGE_REFERENCE);
?TLV_UNPACK_FIELD(submit_sm, source_port, ?SOURCE_PORT);
?TLV_UNPACK_FIELD(submit_sm, source_addr_subunit, ?SOURCE_ADDR_SUBUNIT);
?TLV_UNPACK_FIELD(submit_sm, destination_port, ?DESTINATION_PORT);
?TLV_UNPACK_FIELD(submit_sm, dest_addr_subunit, ?DEST_ADDR_SUBUNIT);
?TLV_UNPACK_FIELD(submit_sm, sar_msg_ref_num, ?SAR_MSG_REF_NUM);
?TLV_UNPACK_FIELD(submit_sm, sar_total_segments, ?SAR_TOTAL_SEGMENTS);
?TLV_UNPACK_FIELD(submit_sm, sar_segment_seqnum, ?SAR_SEGMENT_SEQNUM);
?TLV_UNPACK_FIELD(submit_sm, more_messages_to_send, ?MORE_MESSAGES_TO_SEND);
?TLV_UNPACK_FIELD(submit_sm, payload_type, ?PAYLOAD_TYPE);
?TLV_UNPACK_FIELD(submit_sm, message_payload, ?MESSAGE_PAYLOAD);
?TLV_UNPACK_FIELD(submit_sm, privacy_indicator, ?PRIVACY_INDICATOR);
?TLV_UNPACK_FIELD(submit_sm, callback_num, ?CALLBACK_NUM);
?TLV_UNPACK_FIELD(submit_sm, callback_num_pres_ind, ?CALLBACK_NUM_PRES_IND);
?TLV_UNPACK_FIELD(submit_sm, callback_num_atag, ?CALLBACK_NUM_ATAG);
?TLV_UNPACK_FIELD(submit_sm, source_subaddress, ?SOURCE_SUBADDRESS);
?TLV_UNPACK_FIELD(submit_sm, dest_subaddress, ?DEST_SUBADDRESS);
?TLV_UNPACK_FIELD(submit_sm, user_response_code, ?USER_RESPONSE_CODE);
?TLV_UNPACK_FIELD(submit_sm, display_time, ?DISPLAY_TIME);
?TLV_UNPACK_FIELD(submit_sm, sms_signal, ?SMS_SIGNAL);
?TLV_UNPACK_FIELD(submit_sm, ms_validity, ?MS_VALIDITY);
?TLV_UNPACK_FIELD(submit_sm, ms_msg_wait_facilities, ?MS_MSG_WAIT_FACILITIES);
?TLV_UNPACK_FIELD(submit_sm, number_of_messages, ?NUMBER_OF_MESSAGES);
?TLV_UNPACK_FIELD(submit_sm, alert_on_message_delivery, ?ALERT_ON_MESSAGE_DELIVERY);
?TLV_UNPACK_FIELD(submit_sm, language_indicator, ?LANGUAGE_INDICATOR);
?TLV_UNPACK_FIELD(submit_sm, its_reply_type, ?ITS_REPLY_TYPE);
?TLV_UNPACK_FIELD(submit_sm, its_session_info, ?ITS_SESSION_INFO);
?TLV_UNPACK_FIELD(submit_sm, ussd_service_op, ?USSD_SERVICE_OP);
?TLV_UNPACK_UNEXPECTED().
