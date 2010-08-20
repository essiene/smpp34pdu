-module(tlv).
-include("constants.hrl").
-export([pack/2, unpack/2]).
-export([pack_multi/2]).
-export([pack_noval/1, pack_int/3, pack_cstring/3,
		 pack_octstring_fixedlen/3, pack_octstring_varlen/3,
		 pack_octstring_nomax/2]).

-type(valid_values() :: integer()
					|	iolist()
					|   binary()).

-type(value_list() :: [valid_values()]).

-spec(pack/2 :: (integer(), valid_values()) -> binary()).
-spec(unpack/2 :: (integer(), binary()) -> {valid_values(), binary()}).
-spec(pack_multi/2 :: (integer(), value_list()) -> binary()).
-spec(pack_noval/1 :: (integer()) -> binary()).
-spec(pack_int/3 :: (integer(), integer(), integer()) -> binary()).
-spec(pack_cstring/3 :: (integer(), iolist(), integer()) -> binary()).
-spec(pack_octstring_fixedlen/3 :: (integer(), binary(), integer()) ->
		binary()).
-spec(pack_octstring_varlen/3 :: (integer(), binary(), {integer(), integer()})
		-> binary()).
-spec(pack_octstring_nomax/2 :: (integer(), binary()) -> binary()).

pack(_, undefined) ->
	<<>>;

pack(?DEST_ADDR_SUBUNIT=T, Val) ->
	pack_int(T, Val, 1);

pack(?SOURCE_ADDR_SUBUNIT=T, Val) ->
	pack_int(T, Val, 1);

pack(?DEST_NETWORK_TYPE=T, Val) ->
	pack_int(T, Val, 1);

pack(?SOURCE_NETWORK_TYPE=T, Val) ->
	pack_int(T, Val, 1);

pack(?DEST_BEARER_TYPE=T, Val) ->
	pack_int(T, Val, 1);

pack(?SOURCE_BEARER_TYPE=T, Val) ->
	pack_int(T, Val, 1);

pack(?DEST_TELEMATICS_ID=T, Val) ->
	pack_int(T, Val, 2);

pack(?SOURCE_TELEMATICS_ID=T, Val) ->
	pack_int(T, Val, 1);

pack(?QOS_TIME_TO_LIVE=T, Val) ->
	pack_int(T, Val, 4);

pack(?PAYLOAD_TYPE=T, Val) ->
	pack_int(T, Val, 1);

pack(?ADDITIONAL_STATUS_INFO_TEXT=T, Val) ->
	pack_cstring(T, Val, 256);

pack(?RECEIPTED_MESSAGE_ID=T, Val) ->
	pack_cstring(T, Val, 65);

pack(?MS_MSG_WAIT_FACILITIES=T, Val) ->
	pack_int(T, Val, 1);

pack(?PRIVACY_INDICATOR=T, Val) ->
	pack_int(T, Val, 1);

pack(?SOURCE_SUBADDRESS=T, Val) ->
	pack_octstring_varlen(T, Val, {2, 23});

pack(?DEST_SUBADDRESS=T, Val) ->
	pack_octstring_varlen(T, Val, {2, 23});

pack(?USER_MESSAGE_REFERENCE=T, Val) ->
	pack_int(T, Val, 2);

pack(?USER_RESPONSE_CODE=T, Val) ->
	pack_int(T, Val, 1);

pack(?LANGUAGE_INDICATOR=T, Val) ->
	pack_int(T, Val, 1);

pack(?SOURCE_PORT=T, Val) ->
	pack_int(T, Val, 2);

pack(?DESTINATION_PORT=T, Val) ->
	pack_int(T, Val, 2);

pack(?SAR_MSG_REF_NUM=T, Val) ->
	pack_int(T, Val, 2);

pack(?SAR_TOTAL_SEGMENTS=T, Val) ->
	pack_int(T, Val, 1);

pack(?SAR_SEGMENT_SEQNUM=T, Val) ->
	pack_int(T, Val, 1);

pack(?SC_INTERFACE_VERSION=T, Val) ->
	pack_int(T, Val, 1);

pack(?DISPLAY_TIME=T, Val) ->
	pack_int(T, Val, 1);

pack(?MS_VALIDITY=T, Val) ->
	pack_int(T, Val, 1);

pack(?DPF_RESULT=T, Val) ->
	pack_int(T, Val, 1);

pack(?SET_DPF=T, Val) ->
	pack_int(T, Val, 1);

pack(?MS_AVAILABILITY_STATUS=T, Val) ->
	pack_int(T, Val, 1);

pack(?NETWORK_ERROR_CODE=T, Val) ->
	pack_octstring_fixedlen(T, Val, 3);

pack(?MESSAGE_PAYLOAD=T, Val) ->
	pack_octstring_nomax(T, Val);

pack(?DELIVERY_FAILURE_REASON=T, Val) ->
	pack_int(T, Val, 1);

pack(?MORE_MESSAGES_TO_SEND=T, Val) ->
	pack_int(T, Val, 1);

pack(?MESSAGE_STATE=T, Val) ->
	pack_int(T, Val, 1);

pack(?CALLBACK_NUM=T, Val) ->
	pack_octstring_varlen(T, Val, {4, 19});

pack(?CALLBACK_NUM_PRES_IND=T, Val) ->
	pack_int(T, Val, 1);

pack(?CALLBACK_NUM_ATAG=T, Val) ->
	% the spec says Var max 65. But the description 
	% contains a 1 encoding scheme octet and 
	% then value octets
	pack_octstring_varlen(T, Val, {2, 65});

pack(?NUMBER_OF_MESSAGES=T, Val) ->
	pack_int(T, Val, 1);

pack(?SMS_SIGNAL=T, Val) ->
	pack_int(T, Val, 2);

pack(?ALERT_ON_MESSAGE_DELIVERY=T, _) ->
	pack_noval(T);

pack(?ITS_REPLY_TYPE=T, Val) ->
	pack_int(T, Val, 1);

pack(?ITS_SESSION_INFO=T, Val) ->
	pack_octstring_fixedlen(T, Val, 2);

pack(?USSD_SERVICE_OP=T, Val) ->
	pack_octstring_fixedlen(T, Val, 1).

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
	case pdu_data:octstring_to_bin(Val, Len) of
		{error, _}=Err ->
			Err;
		Bin ->
			<<Tag:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Bin/binary>>
	end.

pack_octstring_varlen(Tag, Val, {Min, Max}) ->
	case pdu_data:octstring_to_bin(Val, {Min, Max}) of
		{error, _}=Err ->
			Err;
		Bin -> 
			Len = 
				case byte_size(Val) of 
					Ln when Ln =< Max ->
						Ln;
					_ ->
						Max
				end,
				<<Tag:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Bin/binary>>
	end.

pack_octstring_nomax(Tag, Val) ->
	Len = byte_size(Val),
	Bin = pdu_data:octstring_to_bin(Val, Len),
	<<Tag:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Bin/binary>>.

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


unpack_int(Tag, <<Tag:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val/binary>>) ->
	pdu_data:bin_to_integer(Val, Len).

unpack_cstring(Tag, <<Tag:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val/binary>>) ->
	pdu_data:bin_to_cstring(Val, Len).

unpack_octstring(Tag, <<Tag:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val/binary>>) ->
	pdu_data:bin_to_octstring(Val, Len).



