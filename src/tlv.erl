-module(tlv).
-include("constants.hrl").
-export[pack/2, unpack/2].


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
	L = [<<?SOURCE_SUBADDRESS:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE>>, pdu_data:octstring_to_bin(Val,23)], % minimum of 2, should be implemented
	list_to_binary(L);

pack(?DEST_SUBADDRESS, Val) ->
	Len = byte_size(Val),
	L = [<<?DEST_SUBADDRESS:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE>>, pdu_data:octstring_to_bin(Val,23)], % minimum of 2, should be implemented
	list_to_binary(L);

pack(?USER_MESSAGE_REFERENCE, Val) ->
	Len = 2,
	Size = Len * ?OCTET_SIZE,
	<<?USER_MESSAGE_REFERENCE:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val:Size>>;

pack(?USER_RESPONSE_CODE, Val) ->
	Len = 1,
	Size = Len * ?OCTET_SIZE,
	<<?USER_RESPONSE_CODE:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val:Size>>;

pack(?SC_INTERFACE_VERSION, Val) ->
	Len = 1,
	Size = Len * ?OCTET_SIZE,
	<<?SOURCE_ADDR_SUBUNIT:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val:Size>>;

pack(?SC_INTERFACE_VERSION, Val) ->
	Len = 1,
	Size = Len * ?OCTET_SIZE,
	<<?SC_INTERFACE_VERSION:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val:Size>>;

pack(?MS_AVAILABILITY_STATUS, Val) ->
	Len = 1,
	Size = Len * ?OCTET_SIZE,
	<<?MS_AVAILABILITY_STATUS:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val:Size>>.

unpack(?DEST_ADDR_SUBUNIT, <<Len:?TLV_LEN_SIZE,Rest0/binary>>) ->
	pdu_data:bin_to_integer(Rest0, Len);

unpack(?SOURCE_ADDR_SUBUNIT, <<Len:?TLV_LEN_SIZE,Rest0/binary>>) ->
	pdu_data:bin_to_integer(Rest0, Len);

unpack(?DEST_NETWORK_TYPE, <<Len:?TLV_LEN_SIZE,Rest0/binary>>) ->
	pdu_data:bin_to_integer(Rest0, Len);

unpack(?SOURCE_NETWORK_TYPE, <<Len:?TLV_LEN_SIZE,Rest0/binary>>) ->
	pdu_data:bin_to_integer(Rest0, Len);

unpack(?DEST_BEARER_TYPE, <<Len:?TLV_LEN_SIZE,Rest0/binary>>) ->
	pdu_data:bin_to_integer(Rest0, Len);

unpack(?SOURCE_BEARER_TYPE, <<Len:?TLV_LEN_SIZE,Rest0/binary>>) ->
	pdu_data:bin_to_integer(Rest0, Len);

unpack(?DEST_TELEMATICS_ID, <<Len:?TLV_LEN_SIZE,Rest0/binary>>) ->
	pdu_data:bin_to_integer(Rest0, Len);

unpack(?SOURCE_TELEMATICS_ID, <<Len:?TLV_LEN_SIZE,Rest0/binary>>) ->
	pdu_data:bin_to_integer(Rest0, Len);

unpack(?QOS_TIME_TO_LIVE, <<Len:?TLV_LEN_SIZE,Rest0/binary>>) ->
	pdu_data:bin_to_integer(Rest0, Len);

unpack(?PAYLOAD_TYPE, <<Len:?TLV_LEN_SIZE,Rest0/binary>>) ->
	pdu_data:bin_to_integer(Rest0, Len);

unpack(?ADDITIONAL_STATUS_INFO_TEXT, <<Len:?TLV_LEN_SIZE,Rest0/binary>>) ->
	pdu_data:bin_to_cstring(Rest0, Len);

unpack(?RECEIPTED_MESSAGE_ID, <<Len:?TLV_LEN_SIZE,Rest0/binary>>) ->
	pdu_data:bin_to_cstring(Rest0, Len);

unpack(?MS_MSG_WAIT_FACILITIES, <<Len:?TLV_LEN_SIZE,Rest0/binary>>) ->
	pdu_data:bin_to_integer(Rest0, Len);

unpack(?PRIVACY_INDICATOR, <<Len:?TLV_LEN_SIZE,Rest0/binary>>) ->
	pdu_data:bin_to_integer(Rest0, Len);

unpack(?SOURCE_SUBADDRESS, <<Len:?TLV_LEN_SIZE,Rest0/binary>>) ->
	pdu_data:bin_to_octstring(Rest0, Len);

unpack(?DEST_SUBADDRESS, <<Len:?TLV_LEN_SIZE,Rest0/binary>>) ->
	pdu_data:bin_to_octstring(Rest0, Len);

unpack(?USER_MESSAGE_REFERENCE, <<Len:?TLV_LEN_SIZE,Rest0/binary>>) ->
	pdu_data:bin_to_integer(Rest0, Len);

unpack(?USER_RESPONSE_CODE, <<Len:?TLV_LEN_SIZE,Rest0/binary>>) ->
	pdu_data:bin_to_integer(Rest0, Len);

unpack(?SC_INTERFACE_VERSION, <<Len:?TLV_LEN_SIZE,Rest0/binary>>) ->
	pdu_data:bin_to_integer(Rest0, Len);

unpack(?SOURCE_PORT, <<Len:?TLV_LEN_SIZE,Rest0/binary>>) ->
	pdu_data:bin_to_integer(Rest0, Len);

unpack(?DESTINATION_PORT, <<Len:?TLV_LEN_SIZE,Rest0/binary>>) ->
	pdu_data:bin_to_integer(Rest0, Len);

unpack(?SAR_MSG_REF_NUM, <<Len:?TLV_LEN_SIZE,Rest0/binary>>) ->
	pdu_data:bin_to_integer(Rest0, Len);

unpack(?SAR_TOTAL_SEGMENTS, <<Len:?TLV_LEN_SIZE,Rest0/binary>>) ->
	pdu_data:bin_to_integer(Rest0, Len);

unpack(?SAR_SEGMENT_SEQNUM, <<Len:?TLV_LEN_SIZE,Rest0/binary>>) ->
	pdu_data:bin_to_integer(Rest0, Len);

unpack(?MORE_MESSAGES_TO_SEND, <<Len:?TLV_LEN_SIZE,Rest0/binary>>) ->
	pdu_data:bin_to_integer(Rest0, Len);

unpack(?MS_AVAILABILITY_STATUS, <<Len:?TLV_LEN_SIZE,Rest0/binary>>) ->
	pdu_data:bin_to_integer(Rest0, Len).
