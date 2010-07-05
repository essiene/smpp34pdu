-module(tlv).
-include("constants.hrl").
-export[pack/2, unpack/2].

-spec(pack/2 :: (integer(), integer()) -> binary()).
-spec(unpack/2 :: (integer(), binary()) -> {integer(), binary()}).

pack(_, undefined) ->
	<<>>;
pack(?SC_INTERFACE_VERSION, Val) ->
	Len = 1,
	Size = Len * ?OCTET_SIZE,
	<<?SC_INTERFACE_VERSION:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val:Size>>.

unpack(?SC_INTERFACE_VERSION, <<Len:16,Rest0/binary>>) ->
	pdu_data:bin_to_integer(Rest0, Len).
