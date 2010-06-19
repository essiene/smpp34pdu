-module(smpp34_BIND_RECEIVER).
-include("pdu.hrl").
-export([pack/1,pack/4]).
-import(pdu_data, [cstring_to_bin/2, integer_to_bin/2]).


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
