-module(tlv).
-include("constants.hrl").
-export[to_bin/2, from_bin/2].

-spec(to_bin/2 :: (integer(), integer()) -> binary()).
-spec(from_bin/2 :: (integer(), binary()) -> {integer(), binary()}).

to_bin(_, undefined) ->
	<<>>;
to_bin(?SC_INTERFACE_VERSION, Val) ->
	Len = 1,
	Size = Len * ?OCTET_SIZE,
	<<?SC_INTERFACE_VERSION:?TLV_TAG_SIZE, Len:?TLV_LEN_SIZE, Val:Size>>.

from_bin(?SC_INTERFACE_VERSION, <<Len:16,Rest0/binary>>) ->
	pdu_data:bin_to_integer(Rest0, Len).
