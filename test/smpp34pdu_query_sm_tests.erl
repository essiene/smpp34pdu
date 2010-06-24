-module(smpp34pdu_query_sm_tests).
-include("../include/pdu.hrl").
-include_lib("eunit/include/eunit.hrl").


query_sm_test_() -> 
    Payload = #query_sm{message_id="abcdefghij",
		source_addr_ton=2,
		source_addr_npi=1,
        source_addr="abcd"},

    Bin = <<97,98,99,100,101,102,103,104,105,106,0,2,1,97,98,99,100,0>>,

	[
		{"Packing Payload will give Bin",
			?_assertEqual(Bin,smpp34pdu_query_sm:pack(Payload))},
		{"Unpacking Bin will give Payload",
			?_assertEqual(Payload, smpp34pdu_query_sm:unpack(Bin))},
		{"Packing and Unpacking Payload will give you Payload", 
			?_assertEqual(Payload,
					smpp34pdu_query_sm:unpack(smpp34pdu_query_sm:pack(Payload)))},
		{"Unpacking and Packing Bin will give you Bin", 
			?_assertEqual(Bin,
					smpp34pdu_query_sm:pack(smpp34pdu_query_sm:unpack(Bin)))}
	].
