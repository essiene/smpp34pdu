-module(tlv).
-include("constants.hrl").
-export([pack/2, unpack/2]).
-export([pack_multi/2]).


-spec(pack/2 :: (integer(), integer()) -> binary()).
-spec(unpack/2 :: (integer(), binary()) -> {integer(), binary()}).

pack(_, undefined) ->
	<<>>;

pack(?DEST_ADDR_SUBUNIT, Val) ->
	Len = 1,
	Size = Len * ?OCTET_SIZE,
	<<?DEST_ADDR_SUBUNIT:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val:Size>>;

pack(?SOURCE_ADDR_SUBUNIT, Val) ->
	Len = 1,
	Size = Len * ?OCTET_SIZE,
	<<?SOURCE_ADDR_SUBUNIT:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val:Size>>;

pack(?DEST_NETWORK_TYPE, Val) ->
	Len = 1,
	Size = Len * ?OCTET_SIZE,
	<<?DEST_NETWORK_TYPE:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val:Size>>;

pack(?SOURCE_NETWORK_TYPE, Val) ->
	Len = 1,
	Size = Len * ?OCTET_SIZE,
	<<?SOURCE_NETWORK_TYPE:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val:Size>>;

pack(?DEST_BEARER_TYPE, Val) ->
	Len = 1,
	Size = Len * ?OCTET_SIZE,
	<<?DEST_BEARER_TYPE:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val:Size>>;

pack(?SOURCE_BEARER_TYPE, Val) ->
	Len = 1,
	Size = Len * ?OCTET_SIZE,
	<<?SOURCE_BEARER_TYPE:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val:Size>>;

pack(?DEST_TELEMATICS_ID, Val) ->
	Len = 2,
	Size = Len * ?OCTET_SIZE,
	<<?DEST_TELEMATICS_ID:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val:Size>>;

pack(?SOURCE_TELEMATICS_ID, Val) ->
	Len = 1,
	Size = Len * ?OCTET_SIZE,
	<<?SOURCE_TELEMATICS_ID:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val:Size>>;

pack(?QOS_TIME_TO_LIVE, Val) ->
	Len = 4,
	Size = Len * ?OCTET_SIZE,
	<<?QOS_TIME_TO_LIVE:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val:Size>>;

pack(?PAYLOAD_TYPE, Val) ->
	Len = 1,
	Size = Len * ?OCTET_SIZE,
	<<?PAYLOAD_TYPE:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val:Size>>;

pack(?ADDITIONAL_STATUS_INFO_TEXT, Val) ->
	Len = length(Val) + 1,
	L = [<<?ADDITIONAL_STATUS_INFO_TEXT:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE>>, pdu_data:cstring_to_bin(Val,256)],
	list_to_binary(L);

pack(?RECEIPTED_MESSAGE_ID, Val) ->
	Len = length(Val) + 1,
	L = [<<?RECEIPTED_MESSAGE_ID:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE>>, pdu_data:cstring_to_bin(Val,65)],
	list_to_binary(L);

pack(?MS_MSG_WAIT_FACILITIES, Val) ->
	Len = 1,
	Size = Len * ?OCTET_SIZE,
	<<?MS_MSG_WAIT_FACILITIES:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val:Size>>;

pack(?PRIVACY_INDICATOR, Val) ->
	Len = 1,
	Size = Len * ?OCTET_SIZE,
	<<?PRIVACY_INDICATOR:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val:Size>>;

pack(?SOURCE_SUBADDRESS, Val) ->
	Len = byte_size(Val),
	L = [<<?SOURCE_SUBADDRESS:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE>>, pdu_data:octstring_to_bin(Val,{2, 23})], 
	list_to_binary(L);

pack(?DEST_SUBADDRESS, Val) ->
	Len = byte_size(Val),
	L = [<<?DEST_SUBADDRESS:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE>>, pdu_data:octstring_to_bin(Val,{2, 23})], 
	list_to_binary(L);

pack(?USER_MESSAGE_REFERENCE, Val) ->
	Len = 2,
	Size = Len * ?OCTET_SIZE,
	<<?USER_MESSAGE_REFERENCE:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val:Size>>;

pack(?USER_RESPONSE_CODE, Val) ->
	Len = 1,
	Size = Len * ?OCTET_SIZE,
	<<?USER_RESPONSE_CODE:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val:Size>>;

pack(?LANGUAGE_INDICATOR, Val) ->
	Len = 1,
	Size = Len * ?OCTET_SIZE,
	<<?LANGUAGE_INDICATOR:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val:Size>>;

pack(?SOURCE_PORT, Val) ->
	Len = 2,
	Size = Len * ?OCTET_SIZE,
	<<?SOURCE_PORT:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val:Size>>;

pack(?DESTINATION_PORT, Val) ->
	Len = 2,
	Size = Len * ?OCTET_SIZE,
	<<?DESTINATION_PORT:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val:Size>>;

pack(?SAR_MSG_REF_NUM, Val) ->
	Len = 2,
	Size = Len * ?OCTET_SIZE,
	<<?SAR_MSG_REF_NUM:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val:Size>>;

pack(?SAR_TOTAL_SEGMENTS, Val) ->
	Len = 1,
	Size = Len * ?OCTET_SIZE,
	<<?SAR_TOTAL_SEGMENTS:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val:Size>>;

pack(?SAR_SEGMENT_SEQNUM, Val) ->
	Len = 1,
	Size = Len * ?OCTET_SIZE,
	<<?SAR_SEGMENT_SEQNUM:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val:Size>>;

pack(?SC_INTERFACE_VERSION, Val) ->
	Len = 1,
	Size = Len * ?OCTET_SIZE,
	<<?SC_INTERFACE_VERSION:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val:Size>>;

pack(?DISPLAY_TIME, Val) ->
	Len = 1,
	Size = Len * ?OCTET_SIZE,
	<<?DISPLAY_TIME:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val:Size>>;

pack(?MS_VALIDITY, Val) ->
	Len = 1,
	Size = Len * ?OCTET_SIZE,
	<<?MS_VALIDITY:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val:Size>>;

pack(?DPF_RESULT, Val) ->
	Len = 1,
	Size = Len * ?OCTET_SIZE,
	<<?DPF_RESULT:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val:Size>>;

pack(?SET_DPF, Val) ->
	Len = 1,
	Size = Len * ?OCTET_SIZE,
	<<?SET_DPF:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val:Size>>;

pack(?MS_AVAILABILITY_STATUS, Val) ->
	Len = 1,
	Size = Len * ?OCTET_SIZE,
	<<?MS_AVAILABILITY_STATUS:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val:Size>>;

pack(?NETWORK_ERROR_CODE, Val) ->
	Len = 3,
	L = [<<?NETWORK_ERROR_CODE:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE>>, pdu_data:octstring_to_bin(Val,3)],
	list_to_binary(L);

pack(?MESSAGE_PAYLOAD, Val) ->
	Len = byte_size(Val),
	L = [<<?MESSAGE_PAYLOAD:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE>>, pdu_data:octstring_to_bin(Val, Len)],
	list_to_binary(L);

pack(?DELIVERY_FAILURE_REASON, Val) ->
	Len = 1,
	Size = Len * ?OCTET_SIZE,
	<<?DELIVERY_FAILURE_REASON:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val:Size>>;

pack(?MORE_MESSAGES_TO_SEND, Val) ->
	Len = 1,
	Size = Len * ?OCTET_SIZE,
	<<?MORE_MESSAGES_TO_SEND:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val:Size>>;

pack(?MESSAGE_STATE, Val) ->
	Len = 1,
	Size = Len * ?OCTET_SIZE,
	<<?MESSAGE_STATE:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val:Size>>;

pack(?CALLBACK_NUM, Val) ->
	Len = byte_size(Val),
	L = [<<?CALLBACK_NUM:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE>>, pdu_data:octstring_to_bin(Val, {4, 19})],
	list_to_binary(L);

pack(?CALLBACK_NUM_PRES_IND, Val) ->
	Len = 1,
	Size = Len * ?OCTET_SIZE,
	<<?CALLBACK_NUM_PRES_IND:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val:Size>>;

pack(?CALLBACK_NUM_ATAG, Val) ->
	Len = byte_size(Val),
	L = [<<?CALLBACK_NUM_ATAG:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE>>,
	pdu_data:octstring_to_bin(Val, {2, 65})], % the spec says Var max 65. But the description contains a 1 encoding scheme octet and then value octets
	list_to_binary(L);

pack(?NUMBER_OF_MESSAGES, Val) ->
	Len = 1,
	Size = Len * ?OCTET_SIZE,
	<<?NUMBER_OF_MESSAGES:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val:Size>>;

pack(?SMS_SIGNAL, Val) ->
	Len = 2,
	Size = Len * ?OCTET_SIZE,
	<<?SMS_SIGNAL:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val:Size>>;

pack(?ALERT_ON_MESSAGE_DELIVERY, Val) ->
	Len = 0,
	Size = Len * ?OCTET_SIZE,
	<<?ALERT_ON_MESSAGE_DELIVERY:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val:Size>>;

pack(?ITS_REPLY_TYPE, Val) ->
	Len = 1,
	Size = Len * ?OCTET_SIZE,
	<<?ITS_REPLY_TYPE:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val:Size>>;

pack(?ITS_SESSION_INFO, Val) ->
	Len = 2,
	L = [<<?ITS_SESSION_INFO:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE>>, pdu_data:octstring_to_bin(Val, 2)],
	list_to_binary(L);

pack(?USSD_SERVICE_OP, Val) ->
	Len = 1,
	L = [<<?USSD_SERVICE_OP:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE>>, pdu_data:octstring_to_bin(Val, 1)],
	list_to_binary(L).


unpack(?DEST_ADDR_SUBUNIT, <<Len:?TLV_LEN_SIZE,Val/binary>>) ->
	pdu_data:bin_to_integer(Val, Len);

unpack(?SOURCE_ADDR_SUBUNIT, <<Len:?TLV_LEN_SIZE,Val/binary>>) ->
	pdu_data:bin_to_integer(Val, Len);

unpack(?DEST_NETWORK_TYPE, <<Len:?TLV_LEN_SIZE,Val/binary>>) ->
	pdu_data:bin_to_integer(Val, Len);

unpack(?SOURCE_NETWORK_TYPE, <<Len:?TLV_LEN_SIZE,Val/binary>>) ->
	pdu_data:bin_to_integer(Val, Len);

unpack(?DEST_BEARER_TYPE, <<Len:?TLV_LEN_SIZE,Val/binary>>) ->
	pdu_data:bin_to_integer(Val, Len);

unpack(?SOURCE_BEARER_TYPE, <<Len:?TLV_LEN_SIZE,Val/binary>>) ->
	pdu_data:bin_to_integer(Val, Len);

unpack(?DEST_TELEMATICS_ID, <<Len:?TLV_LEN_SIZE,Val/binary>>) -> 
	pdu_data:bin_to_integer(Val, Len);

unpack(?SOURCE_TELEMATICS_ID, <<Len:?TLV_LEN_SIZE,Val/binary>>) ->
	pdu_data:bin_to_integer(Val, Len);

unpack(?QOS_TIME_TO_LIVE, <<Len:?TLV_LEN_SIZE,Val/binary>>) ->	
	pdu_data:bin_to_integer(Val, Len);

unpack(?PAYLOAD_TYPE, <<Len:?TLV_LEN_SIZE,Val/binary>>) ->
	pdu_data:bin_to_integer(Val, Len);

unpack(?ADDITIONAL_STATUS_INFO_TEXT, <<Len:?TLV_LEN_SIZE,Val/binary>>) ->
	pdu_data:bin_to_cstring(Val, Len);

unpack(?RECEIPTED_MESSAGE_ID, <<Len:?TLV_LEN_SIZE,Val/binary>>) ->
	pdu_data:bin_to_cstring(Val, Len);

unpack(?MS_MSG_WAIT_FACILITIES, <<Len:?TLV_LEN_SIZE,Val/binary>>) ->	
	pdu_data:bin_to_integer(Val, Len);

unpack(?PRIVACY_INDICATOR, <<Len:?TLV_LEN_SIZE,Val/binary>>) ->
	pdu_data:bin_to_integer(Val, Len);

unpack(?SOURCE_SUBADDRESS, <<Len:?TLV_LEN_SIZE,Val/binary>>) ->
	pdu_data:bin_to_octstring(Val, Len);

unpack(?DEST_SUBADDRESS, <<Len:?TLV_LEN_SIZE,Val/binary>>) ->
	pdu_data:bin_to_octstring(Val, Len);

unpack(?USER_MESSAGE_REFERENCE, <<Len:?TLV_LEN_SIZE,Val/binary>>) ->
	pdu_data:bin_to_integer(Val, Len);

unpack(?USER_RESPONSE_CODE, <<Len:?TLV_LEN_SIZE,Val/binary>>) ->
	pdu_data:bin_to_integer(Val, Len);

unpack(?LANGUAGE_INDICATOR, <<Len:?TLV_LEN_SIZE,Val/binary>>) ->
	pdu_data:bin_to_integer(Val, Len);

unpack(?SOURCE_PORT, <<Len:?TLV_LEN_SIZE,Val/binary>>) ->
	pdu_data:bin_to_integer(Val, Len);

unpack(?DESTINATION_PORT, <<Len:?TLV_LEN_SIZE,Val/binary>>) ->
	pdu_data:bin_to_integer(Val, Len);

unpack(?SAR_MSG_REF_NUM, <<Len:?TLV_LEN_SIZE,Val/binary>>) ->
	pdu_data:bin_to_integer(Val, Len);

unpack(?SAR_TOTAL_SEGMENTS, <<Len:?TLV_LEN_SIZE,Val/binary>>) ->
	pdu_data:bin_to_integer(Val, Len);

unpack(?SAR_SEGMENT_SEQNUM, <<Len:?TLV_LEN_SIZE,Val/binary>>) ->
	pdu_data:bin_to_integer(Val, Len);

unpack(?SC_INTERFACE_VERSION, <<Len:?TLV_LEN_SIZE,Val/binary>>) ->
	pdu_data:bin_to_integer(Val, Len);

unpack(?MS_VALIDITY, <<Len:?TLV_LEN_SIZE,Val/binary>>) -> 
	pdu_data:bin_to_integer(Val, Len);

unpack(?DISPLAY_TIME, <<Len:?TLV_LEN_SIZE,Val/binary>>) ->
	pdu_data:bin_to_integer(Val, Len);

unpack(?DPF_RESULT, <<Len:?TLV_LEN_SIZE,Val/binary>>) ->
	pdu_data:bin_to_integer(Val, Len);

unpack(?SET_DPF, <<Len:?TLV_LEN_SIZE,Val/binary>>) ->
	pdu_data:bin_to_integer(Val, Len);

unpack(?MS_AVAILABILITY_STATUS, <<Len:?TLV_LEN_SIZE,Val/binary>>) ->
	pdu_data:bin_to_integer(Val, Len);

unpack(?NETWORK_ERROR_CODE, <<Len:?TLV_LEN_SIZE,Val/binary>>) ->
	pdu_data:bin_to_octstring(Val, Len);

unpack(?MESSAGE_PAYLOAD, <<Len:?TLV_LEN_SIZE,Val/binary>>) ->
	pdu_data:bin_to_octstring(Val, Len);

unpack(?DELIVERY_FAILURE_REASON, <<Len:?TLV_LEN_SIZE,Val/binary>>) ->
	pdu_data:bin_to_integer(Val, Len);

unpack(?MORE_MESSAGES_TO_SEND, <<Len:?TLV_LEN_SIZE,Val/binary>>) ->
	pdu_data:bin_to_integer(Val, Len);

unpack(?MESSAGE_STATE, <<Len:?TLV_LEN_SIZE,Val/binary>>) ->
	pdu_data:bin_to_integer(Val, Len);

unpack(?CALLBACK_NUM, <<Len:?TLV_LEN_SIZE,Val/binary>>) ->
	pdu_data:bin_to_octstring(Val, Len);

unpack(?CALLBACK_NUM_PRES_IND, <<Len:?TLV_LEN_SIZE,Val/binary>>) ->
	pdu_data:bin_to_integer(Val, Len);

unpack(?CALLBACK_NUM_ATAG, <<Len:?TLV_LEN_SIZE,Val/binary>>) ->
	pdu_data:bin_to_octstring(Val, Len);

unpack(?NUMBER_OF_MESSAGES, <<Len:?TLV_LEN_SIZE,Val/binary>>) ->
	pdu_data:bin_to_integer(Val, Len);

unpack(?SMS_SIGNAL, <<Len:?TLV_LEN_SIZE,Val/binary>>) ->
	pdu_data:bin_to_integer(Val, Len);

unpack(?ALERT_ON_MESSAGE_DELIVERY, <<Len:?TLV_LEN_SIZE,Val/binary>>) ->
	pdu_data:bin_to_octstring(Val, Len);

unpack(?ITS_REPLY_TYPE, <<Len:?TLV_LEN_SIZE,Val/binary>>) ->
	pdu_data:bin_to_integer(Val, Len);

unpack(?ITS_SESSION_INFO, <<Len:?TLV_LEN_SIZE,Val/binary>>) ->
	pdu_data:bin_to_octstring(Val, Len);

unpack(?USSD_SERVICE_OP, <<Len:?TLV_LEN_SIZE,Val/binary>>) ->
	pdu_data:bin_to_octstring(Val, Len).


pack_multi(_, []) ->
	<<>>;
pack_multi(Tag, [_|_]=L) ->
	pack_multi(Tag, L, <<>>).

pack_multi(__, [], Accm) ->
	Accm;
pack_multi(Tag, [Head|Rest], Accm) ->
	Bin = pack(Tag, Head),
	pack_multi(Tag, Rest, <<Accm/binary, Bin/binary>>).


pack_noval(Tag) ->
	<<Tag:?TLV_TAG_SIZE, 0:?TLV_LEN_SIZE>>.

pack_int(Tag, Val, Len) ->
	Size = Len * ?OCTET_SIZE,
	<<Tag:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val:Size>>.

pack_octstring_fixedlen(Tag, Val, Len) ->
	L = [<<Tag:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE>>, pdu_data:octstring_to_bin(Val, Len)],
	list_to_binary(L).

pack_octstring_varlen(Tag, Val, {Min, Max}) ->
	Len = byte_size(Val),
	L = [<<Tag:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE>>, pdu_data:octstring_to_bin(Val, {Min, Max})], 	
	list_to_binary(L).

pack_octstring_nomax(Tag, Val) ->
	Len = byte_size(Val),
	L = [<<Tag:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE>>, pdu_data:octstring_to_bin(Val, Len)],
	list_to_binary(L).

pack_cstring(Tag, Val, Max) ->
	Len = 
		case length(Val) of 
			Ln when Ln =< Max-1 ->
				Ln+1;
			_ ->
				Max
		end,

	L = [<<Tag:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE>>, pdu_data:cstring_to_bin(Val,Max)],
	list_to_binary(L).
