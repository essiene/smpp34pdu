-module(smpp34pdu_cancel_sm_tests).
-include("../include/pdu.hrl").
-include_lib("eunit/include/eunit.hrl").


cancel_sm_test_() -> 
    Payload = #cancel_sm{service_type="WAP",
		message_id="abcdefghij",
		source_addr_ton=2,
		source_addr_npi=1,
        source_addr="abcd",
		dest_addr_ton=2,
		dest_addr_npi=1,
		destination_addr="efgh"},

    Bin = <<87,65,80,0,97,98,99,100,101,102,103,104,105,106,0,2,
			1,97,98,99,100,0,2,1,101,102,103,104,0>>,

	[
		{"Packing Payload will give Bin",
			?_assertEqual(Bin,smpp34pdu_cancel_sm:pack(Payload))},
		{"Unpacking Bin will give Payload",
			?_assertEqual(Payload, smpp34pdu_cancel_sm:unpack(Bin))},
		{"Packing and Unpacking Payload will give you Payload", 
			?_assertEqual(Payload,
					smpp34pdu_cancel_sm:unpack(smpp34pdu_cancel_sm:pack(Payload)))},
		{"Unpacking and Packing Bin will give you Bin", 
			?_assertEqual(Bin,
					smpp34pdu_cancel_sm:pack(smpp34pdu_cancel_sm:unpack(Bin)))}
	].
