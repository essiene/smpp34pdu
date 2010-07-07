-module(smpp34pdu_query_sm_resp_tests).
-include("../include/smpp34pdu.hrl").
-include_lib("eunit/include/eunit.hrl").


query_sm_test_() -> 
    Payload = #query_sm_resp{message_id="abcdefghij",
		final_date="100702115500000+",
		message_state=3,
        error_code=5},

    Bin =
	<<97,98,99,100,101,102,103,104,105,106,0,49,48,48,55,48,50,49,49,53,53,48,48,48,48,48,43,0,3,5>>,

	[
		{"Packing Payload will give Bin",
			?_assertEqual(Bin,smpp34pdu_query_sm_resp:pack(Payload))},
		{"Unpacking Bin will give Payload",
			?_assertEqual(Payload, smpp34pdu_query_sm_resp:unpack(Bin))},
		{"Packing and Unpacking Payload will give you Payload", 
			?_assertEqual(Payload,
					smpp34pdu_query_sm_resp:unpack(smpp34pdu_query_sm_resp:pack(Payload)))},
		{"Unpacking and Packing Bin will give you Bin", 
			?_assertEqual(Bin,
					smpp34pdu_query_sm_resp:pack(smpp34pdu_query_sm_resp:unpack(Bin)))}
	].
