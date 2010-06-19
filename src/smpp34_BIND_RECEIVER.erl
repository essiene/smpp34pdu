-module(smpp34_BIND_RECEIVER).
-include("pdu.hrl").
-export([pack/1,unpack/1]).
-import(pdu_data, [cstring_to_bin/2, integer_to_bin/2]).
-import(pdu_data, [bin_to_cstring/2, bin_to_integer/2]).


pack(#bind_receiver{system_id=SystemId, 
		password=Password, 
		system_type=SystemType, 
		interface_version=InterfaceVersion, 
		addr_ton=AddrTon, 
		addr_npi=AddrNpi, 
		address_range=AddressRange}) ->

		L = [cstring_to_bin(SystemId, 16),
					   cstring_to_bin(Password, 9),
					   cstring_to_bin(SystemType, 13),
					   integer_to_bin(InterfaceVersion, 1),
					   integer_to_bin(AddrTon, 1),
					   integer_to_bin(AddrNpi, 1),
					   cstring_to_bin(AddressRange, 41)],

		list_to_binary(L).


unpack(Bin0) ->
	{SystemId, Bin1} = bin_to_cstring(Bin0, 16),
	{Password, Bin2} = bin_to_cstring(Bin1, 9),
	{SystemType, Bin3} = bin_to_cstring(Bin2, 13),
	{InterfaceVersion, Bin4} = bin_to_integer(Bin3, 1),
	{AddrTon, Bin5} = bin_to_integer(Bin4, 1),
	{AddrNpi, Bin6} = bin_to_integer(Bin5, 1),
	{AddressRange, <<>>} = bin_to_cstring(Bin6, 41),

	#bind_receiver {system_id=SystemId,
		password=Password,
		system_type=SystemType,
		interface_version=InterfaceVersion,
		addr_ton=AddrTon,
		addr_npi=AddrNpi,
		address_range=AddressRange}.

