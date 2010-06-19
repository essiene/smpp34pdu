-module(smpp34_BIND_RECEIVER_tests).
-include("../include/pdu.hrl").
-include_lib("eunit/include/eunit.hrl").


bind_receiver_pack_test_() -> 
    Payload = #bind_receiver{system_id="abcdefghij",
        password="abcd",
        addr_ton=2,
        addr_npi=1},

    Bin = <<97,98,99,100,101,102,103,104,105,106,0,97,98,99,100,0,0,52,2,1,0>>,

	Packed = smpp34_BIND_RECEIVER:pack(Payload),

	?_assertEqual(Bin,Packed).

bind_receiver_unpack_test_() -> 
    Payload = #bind_receiver{system_id="abcdefghij",
        password="abcd",
		system_type="",
		interface_version=?VERSION,
        addr_ton=2,
        addr_npi=1,
		address_range=""},

    Bin = <<97,98,99,100,101,102,103,104,105,106,0,97,98,99,100,0,0,52,2,1,0>>,
	
	UnPacked = smpp34_BIND_RECEIVER:unpack(Bin),

	?_assertEqual(Payload,UnPacked).

cyclic_test_() ->
    Payload = #bind_receiver{system_id="abcdefghij",
        password="abcd",
        addr_ton=2,
        addr_npi=1},

	?_assertEqual(Payload, smpp34_BIND_RECEIVER:unpack(smpp34_BIND_RECEIVER:pack(Payload))).

