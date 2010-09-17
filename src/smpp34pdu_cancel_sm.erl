-module(smpp34pdu_cancel_sm).
-include("smpp34pdu.hrl").
-include("types.hrl").
-export([pack/1,unpack/1]).
-import(pdu_data, [cstring_to_bin/2, integer_to_bin/2]).
-import(pdu_data, [bin_to_cstring/2, bin_to_integer/2]).

-spec(pack/1 :: (cancel_sm()) -> binary()).
-spec(unpack/1 :: (binary()) -> cancel_sm()).

pack(#cancel_sm{service_type=ServiceType,
		message_id=MessageId, 
		source_addr_ton=SrcAddrTon,
		source_addr_npi=SrcAddrNpi,
		source_addr=SrcAddr,
		dest_addr_ton=DestAddrTon,
		dest_addr_npi=DestAddrNpi,
		destination_addr=DestAddr}) ->


		L = [cstring_to_bin(ServiceType, 6), 
					   cstring_to_bin(MessageId, 65),
					   integer_to_bin(SrcAddrTon, 1),
					   integer_to_bin(SrcAddrNpi, 1),
					   cstring_to_bin(SrcAddr, 21),
					   integer_to_bin(DestAddrTon, 1),
					   integer_to_bin(DestAddrNpi, 1),
					   cstring_to_bin(DestAddr, 21)],

		list_to_binary(L).


unpack(Bin0) ->
	{ServiceType, Bin1} = bin_to_cstring(Bin0, 6),
	{MessageId, Bin2} = bin_to_cstring(Bin1, 65),
	{SrcAddrTon, Bin3} = bin_to_integer(Bin2, 1),
	{SrcAddrNpi, Bin4} = bin_to_integer(Bin3, 1),
	{SrcAddr, Bin5} = bin_to_cstring(Bin4, 21),
	{DestAddrTon, Bin6} = bin_to_integer(Bin5, 1),
	{DestAddrNpi, Bin7} = bin_to_integer(Bin6, 1),
	{DestAddr, _} = bin_to_cstring(Bin7, 21),

	#cancel_sm {service_type=ServiceType,
		message_id=MessageId,
		source_addr_ton=SrcAddrTon,
		source_addr_npi=SrcAddrNpi,
		source_addr=SrcAddr,
		dest_addr_ton=DestAddrTon,
		dest_addr_npi=DestAddrNpi,
		destination_addr=DestAddr}.
