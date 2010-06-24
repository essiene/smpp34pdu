-module(smpp34pdu_outbind_tests).
-include("../include/pdu.hrl").
-include_lib("eunit/include/eunit.hrl").


outbind_test_() -> 
    Payload = #outbind{system_id="abcdefghij",
        password="abcd"},

    Bin = <<97,98,99,100,101,102,103,104,105,106,0,97,98,99,100,0>>,

	[
		{"Packing Payload will give Bin",
			?_assertEqual(Bin,smpp34pdu_outbind:pack(Payload))},
		{"Unpacking Bin will give Payload",
			?_assertEqual(Payload, smpp34pdu_outbind:unpack(Bin))},
		{"Packing and Unpacking Payload will give you Payload", 
			?_assertEqual(Payload,
					smpp34pdu_outbind:unpack(smpp34pdu_outbind:pack(Payload)))},
		{"Unpacking and Packing Bin will give you Bin", 
			?_assertEqual(Bin,
					smpp34pdu_outbind:pack(smpp34pdu_outbind:unpack(Bin)))}
	].
