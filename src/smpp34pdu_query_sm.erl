-module(smpp34pdu_query_sm).
-include("pdu.hrl").
-include("types.hrl").
-export([pack/1,unpack/1]).
-import(pdu_data, [cstring_to_bin/2, integer_to_bin/2]).
-import(pdu_data, [bin_to_cstring/2, bin_to_integer/2]).

-spec(pack/1 :: (query_sm()) -> binary()).
-spec(unpack/1 :: (binary()) -> query_sm()).

pack(#query_sm{message_id=MessageId, 
		source_addr_ton=SrcAddrTon,
		source_addr_npi=SrcAddrNpi,
		source_addr=SrcAddr}) ->

		L = [cstring_to_bin(MessageId, 65),
					   integer_to_bin(SrcAddrTon, 1),
					   integer_to_bin(SrcAddrNpi, 1),
					   cstring_to_bin(SrcAddr, 21)],

		list_to_binary(L).


unpack(Bin0) ->
	{MessageId, Bin1} = bin_to_cstring(Bin0, 65),
	{SrcAddrTon, Bin2} = bin_to_integer(Bin1, 1),
	{SrcAddrNpi, Bin3} = bin_to_integer(Bin2, 1),
	{SrcAddr, <<>>} = bin_to_cstring(Bin3, 21),

	#query_sm {message_id=MessageId,
		source_addr_ton=SrcAddrTon,
		source_addr_npi=SrcAddrNpi,
		source_addr=SrcAddr}.

