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

pack(?MS_AVAILABILITY_STATUS, Val) ->
	Len = 1,
	Size = Len * ?OCTET_SIZE,
	<<?MS_AVAILABILITY_STATUS:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val:Size>>.

unpack(?DEST_ADDR_SUBUNIT, <<Len:16,Rest0/binary>>) ->
	pdu_data:bin_to_integer(Rest0, Len);

unpack(?SOURCE_ADDR_SUBUNIT, <<Len:16,Rest0/binary>>) ->
	pdu_data:bin_to_integer(Rest0, Len);

unpack(?SC_INTERFACE_VERSION, <<Len:16,Rest0/binary>>) ->
	pdu_data:bin_to_integer(Rest0, Len);

unpack(?MS_AVAILABILITY_STATUS, <<Len:?TLV_LEN_SIZE,Rest0/binary>>) ->
	pdu_data:bin_to_integer(Rest0, Len).
