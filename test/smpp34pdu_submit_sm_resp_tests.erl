-module(smpp34pdu_submit_sm_resp_tests).
-include("../include/smpp34pdu.hrl").
-include_lib("eunit/include/eunit.hrl").


submit_sm_resp_test_() -> 
    Payload = #submit_sm_resp{message_id="abcdefghij"},

    Bin = <<97,98,99,100,101,102,103,104,105,106,0>>,

	[
		{"Packing Payload will give Bin",
			?_assertEqual(Bin,smpp34pdu_submit_sm_resp:pack(Payload))},
		{"Unpacking Bin will give Payload",
			?_assertEqual(Payload, smpp34pdu_submit_sm_resp:unpack(Bin))},
		{"Packing and Unpacking Payload will give you Payload", 
			?_assertEqual(Payload,
					smpp34pdu_submit_sm_resp:unpack(smpp34pdu_submit_sm_resp:pack(Payload)))},
		{"Unpacking and Packing Bin will give you Bin", 
			?_assertEqual(Bin,
					smpp34pdu_submit_sm_resp:pack(smpp34pdu_submit_sm_resp:unpack(Bin)))}
	].
