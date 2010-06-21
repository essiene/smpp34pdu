-module(smpp34_BIND_TRANSMITTER_tests).
-include("../include/pdu.hrl").
-include_lib("eunit/include/eunit.hrl").


bind_transmitter_test_() -> 
    Payload = #bind_transmitter{system_id="abcdefghij",
        password="abcd",
		system_type="",
		interface_version=?VERSION,
        addr_ton=2,
        addr_npi=1,
		address_range=""},

    Bin = <<97,98,99,100,101,102,103,104,105,106,0,97,98,99,100,0,0,52,2,1,0>>,

	[
		{"Packing Payload will give Bin",
			?_assertEqual(Bin,smpp34_BIND_TRANSMITTER:pack(Payload))},
		{"Unpacking Bin will give Payload",
			?_assertEqual(Payload, smpp34_BIND_TRANSMITTER:unpack(Bin))},
		{"Packing and Unpacking Payload will give you Payload", 
			?_assertEqual(Payload,
					smpp34_BIND_TRANSMITTER:unpack(smpp34_BIND_TRANSMITTER:pack(Payload)))},
		{"Unpacking and Packing Bin will give you Bin", 
			?_assertEqual(Bin,
					smpp34_BIND_TRANSMITTER:pack(smpp34_BIND_TRANSMITTER:unpack(Bin)))}
	].
