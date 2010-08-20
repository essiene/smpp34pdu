-module(tlv).
-include("constants.hrl").
-export([pack/2, unpack/2]).
-export([pack_multi/2, unpack_multi/2]).
-export([pack_noval/1, pack_int/3, pack_cstring/3,
		 pack_octstring_fixedlen/3, pack_octstring_varlen/3,
		 pack_octstring_nomax/2]).
-export([unpack_int/2, unpack_cstring/2, unpack_octstring/2]).

-type(valid_values() :: integer()
					|	iolist()
					|   binary()).

-type(value_list() :: [valid_values()]).

-spec(pack/2 :: (integer(), valid_values()) -> binary()).
-spec(unpack/2 :: (integer(), binary()) -> {valid_values(), binary()}).
-spec(pack_multi/2 :: (integer(), value_list()) -> binary()).
-spec(unpack_multi/2 :: (integer(), binary()) -> value_list()).
-spec(pack_noval/1 :: (integer()) -> binary()).
-spec(pack_int/3 :: (integer(), integer(), integer()) -> binary()).
-spec(pack_cstring/3 :: (integer(), iolist(), integer()) -> binary()).
-spec(pack_octstring_fixedlen/3 :: (integer(), binary(), integer()) ->
		binary()).
-spec(pack_octstring_varlen/3 :: (integer(), binary(), {integer(), integer()})
		-> binary()).
-spec(pack_octstring_nomax/2 :: (integer(), binary()) -> binary()).
-spec(unpack_int/2 :: (integer(), binary()) -> integer()).
-spec(unpack_cstring/2 :: (integer(), binary()) -> iolist()).
-spec(unpack_octstring/2 :: (integer(), binary()) -> binary()).

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



%% TODO: We should stop being nice,
%% We should enforce lengths when unpacking
%% too, just like we do when packing

unpack(?DEST_ADDR_SUBUNIT=T, Bin) ->
	unpack_int(T, Bin);

unpack(?SOURCE_ADDR_SUBUNIT=T, Bin) ->
	unpack_int(T, Bin);

unpack(?DEST_NETWORK_TYPE=T, Bin) ->
	unpack_int(T, Bin);

unpack(?SOURCE_NETWORK_TYPE=T, Bin) ->
	unpack_int(T, Bin);

unpack(?DEST_BEARER_TYPE=T, Bin) ->
	unpack_int(T, Bin);

unpack(?SOURCE_BEARER_TYPE=T, Bin) ->
	unpack_int(T, Bin);

unpack(?DEST_TELEMATICS_ID=T, Bin) ->
	unpack_int(T, Bin);

unpack(?SOURCE_TELEMATICS_ID=T, Bin) ->
	unpack_int(T, Bin);

unpack(?QOS_TIME_TO_LIVE=T, Bin) ->
	unpack_int(T, Bin);

unpack(?PAYLOAD_TYPE=T, Bin) ->
	unpack_int(T, Bin);

unpack(?ADDITIONAL_STATUS_INFO_TEXT=T, Bin) ->
	unpack_cstring(T, Bin);

unpack(?RECEIPTED_MESSAGE_ID=T, Bin) ->
	unpack_cstring(T, Bin);

unpack(?MS_MSG_WAIT_FACILITIES=T, Bin) ->
	unpack_int(T, Bin);

unpack(?PRIVACY_INDICATOR=T, Bin) ->
	unpack_int(T, Bin);

unpack(?SOURCE_SUBADDRESS=T, Bin) ->
	unpack_octstring(T, Bin);

unpack(?DEST_SUBADDRESS=T, Bin) ->
	unpack_octstring(T, Bin);

unpack(?USER_MESSAGE_REFERENCE=T, Bin) ->
	unpack_int(T, Bin);

unpack(?USER_RESPONSE_CODE=T, Bin) ->
	unpack_int(T, Bin);

unpack(?LANGUAGE_INDICATOR=T, Bin) ->
	unpack_int(T, Bin);

unpack(?SOURCE_PORT=T, Bin) ->
	unpack_int(T, Bin);

unpack(?DESTINATION_PORT=T, Bin) ->
	unpack_int(T, Bin);

unpack(?SAR_MSG_REF_NUM=T, Bin) ->
	unpack_int(T, Bin);

unpack(?SAR_TOTAL_SEGMENTS=T, Bin) ->
	unpack_int(T, Bin);

unpack(?SAR_SEGMENT_SEQNUM=T, Bin) ->
	unpack_int(T, Bin);

unpack(?SC_INTERFACE_VERSION=T, Bin) ->
	unpack_int(T, Bin);

unpack(?MS_VALIDITY=T, Bin) ->
	unpack_int(T, Bin);

unpack(?DISPLAY_TIME=T, Bin) ->
	unpack_int(T, Bin);

unpack(?DPF_RESULT=T, Bin) ->
	unpack_int(T, Bin);

unpack(?SET_DPF=T, Bin) ->
	unpack_int(T, Bin);

unpack(?MS_AVAILABILITY_STATUS=T, Bin) ->
	unpack_int(T, Bin);

unpack(?NETWORK_ERROR_CODE=T, Bin) ->
	unpack_octstring(T, Bin);

unpack(?MESSAGE_PAYLOAD=T, Bin) ->
	unpack_octstring(T, Bin);

unpack(?DELIVERY_FAILURE_REASON=T, Bin) ->
	unpack_int(T, Bin);

unpack(?MORE_MESSAGES_TO_SEND=T, Bin) ->
	unpack_int(T, Bin);

unpack(?MESSAGE_STATE=T, Bin) ->
	unpack_int(T, Bin);

unpack(?CALLBACK_NUM=T, Bin) ->
	unpack_octstring(T, Bin);

unpack(?CALLBACK_NUM_PRES_IND=T, Bin) ->
	unpack_int(T, Bin);

unpack(?CALLBACK_NUM_ATAG=T, Bin) ->
	unpack_octstring(T, Bin);

unpack(?NUMBER_OF_MESSAGES=T, Bin) ->
	unpack_int(T, Bin);

unpack(?SMS_SIGNAL=T, Bin) ->
	unpack_int(T, Bin);

unpack(?ALERT_ON_MESSAGE_DELIVERY=T, Bin) ->
	unpack_octstring(T, Bin);

unpack(?ITS_REPLY_TYPE=T, Bin) ->
	unpack_int(T, Bin);

unpack(?ITS_SESSION_INFO=T, Bin) ->
	unpack_octstring(T, Bin);

unpack(?USSD_SERVICE_OP=T, Bin) ->
	unpack_octstring(T, Bin).


pack_multi(_, []) ->
	<<>>;
pack_multi(Tag, [_|_]=L) ->
	pack_multi(Tag, L, <<>>).

pack_multi(__, [], Accm) ->
	Accm;
pack_multi(Tag, [Head|Rest], Accm) ->
	Bin = pack(Tag, Head),
	pack_multi(Tag, Rest, <<Accm/binary, Bin/binary>>).


unpack_multi(_, <<>>) ->
	[];
unpack_multi(Tag, Bin) ->
	unpack_multi(Tag, Bin, []).

unpack_multi(_, <<>>, Accm) ->
	lists:reverse(Accm);
unpack_multi(Tag, Bin, Accm) ->
	{Value, Rest} = unpack(Tag, Bin),
	unpack_multi(Tag, Rest, [Value|Accm]).


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
