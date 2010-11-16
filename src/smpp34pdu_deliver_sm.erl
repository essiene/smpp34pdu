-module(smpp34pdu_deliver_sm).
-include("smpp34pdu.hrl").
-include("types.hrl").
-include("tlv_macros.hrl").

-export([pack/1,unpack/1]).
-import(pdu_data, [cstring_to_bin/2, string_to_bin/2, integer_to_bin/2]).
-import(pdu_data, [bin_to_cstring/2, bin_to_string/2, bin_to_integer/2]).

-spec(pack/1 :: (deliver_sm()) -> binary()).
-spec(unpack/1 :: (binary()) -> deliver_sm()).

pack(#deliver_sm{service_type=SrvType,
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
		destination_port=DstPort, 
		sar_msg_ref_num=SarMsgRefNum, 
		sar_total_segments=SarTotalSegments, 
		sar_segment_seqnum=SarSegSeqnum, 
		user_response_code=UserResponseCode,
		privacy_indicator=PrivacyIndicator,
		payload_type=PayloadType,
		message_payload=MessagePayload,
		callback_num=CallbackNum,
		source_subaddress=SourceSubaddress,
		dest_subaddress=DestSubaddress,
		language_indicator=LanguageIndicator,
		its_session_info=ItsSessionInfo,
		network_error_code=NetworkErrCode,
		message_state=MessageState,
		receipted_message_id=ReceiptedMsgId}) ->

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
					   tlv:pack(?DESTINATION_PORT, DstPort), 
					   tlv:pack(?SAR_MSG_REF_NUM, SarMsgRefNum), 
					   tlv:pack(?SAR_TOTAL_SEGMENTS, SarTotalSegments), 
					   tlv:pack(?SAR_SEGMENT_SEQNUM, SarSegSeqnum), 
					   tlv:pack(?USER_RESPONSE_CODE, UserResponseCode),
					   tlv:pack(?PRIVACY_INDICATOR, PrivacyIndicator),
					   tlv:pack(?PAYLOAD_TYPE, PayloadType),
					   tlv:pack(?MESSAGE_PAYLOAD, MessagePayload),
					   tlv:pack_multi(?CALLBACK_NUM, CallbackNum),
					   tlv:pack(?SOURCE_SUBADDRESS, SourceSubaddress),
					   tlv:pack(?DEST_SUBADDRESS, DestSubaddress),
					   tlv:pack(?LANGUAGE_INDICATOR, LanguageIndicator),
					   tlv:pack(?ITS_SESSION_INFO, ItsSessionInfo),
					   tlv:pack(?NETWORK_ERROR_CODE, NetworkErrCode),
					   tlv:pack(?MESSAGE_STATE, MessageState),
					   tlv:pack(?RECEIPTED_MESSAGE_ID, ReceiptedMsgId)],

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

	unpack_tlv_fields(Bin18, #deliver_sm{service_type=SrvType,
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
?TLV_UNPACK_FIELD(deliver_sm, user_message_reference, ?USER_MESSAGE_REFERENCE);
?TLV_UNPACK_FIELD(deliver_sm, source_port, ?SOURCE_PORT);
?TLV_UNPACK_FIELD(deliver_sm, destination_port, ?DESTINATION_PORT);
?TLV_UNPACK_FIELD(deliver_sm, sar_msg_ref_num, ?SAR_MSG_REF_NUM);
?TLV_UNPACK_FIELD(deliver_sm, sar_total_segments, ?SAR_TOTAL_SEGMENTS);
?TLV_UNPACK_FIELD(deliver_sm, sar_segment_seqnum, ?SAR_SEGMENT_SEQNUM);
?TLV_UNPACK_FIELD(deliver_sm, user_response_code, ?USER_RESPONSE_CODE);
?TLV_UNPACK_FIELD(deliver_sm, privacy_indicator, ?PRIVACY_INDICATOR);
?TLV_UNPACK_FIELD(deliver_sm, payload_type, ?PAYLOAD_TYPE);
?TLV_UNPACK_FIELD(deliver_sm, message_payload, ?MESSAGE_PAYLOAD);
?TLV_UNPACK_FIELD(deliver_sm, callback_num, ?CALLBACK_NUM);
?TLV_UNPACK_FIELD(deliver_sm, source_subaddress, ?SOURCE_SUBADDRESS);
?TLV_UNPACK_FIELD(deliver_sm, dest_subaddress, ?DEST_SUBADDRESS);
?TLV_UNPACK_FIELD(deliver_sm, language_indicator, ?LANGUAGE_INDICATOR);
?TLV_UNPACK_FIELD(deliver_sm, its_session_info, ?ITS_SESSION_INFO);
?TLV_UNPACK_FIELD(deliver_sm, network_error_code, ?NETWORK_ERROR_CODE);
?TLV_UNPACK_FIELD(deliver_sm, message_state, ?MESSAGE_STATE);
?TLV_UNPACK_FIELD(deliver_sm, receipted_message_id, ?RECEIPTED_MESSAGE_ID);
?TLV_UNPACK_UNEXPECTED().
