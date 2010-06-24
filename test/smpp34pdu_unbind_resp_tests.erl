-module(smpp34pdu_unbind_resp_tests).
-include("../include/pdu.hrl").
-include_lib("eunit/include/eunit.hrl").


unbind_resp_test_() -> 
    Payload = #unbind_resp{},
    Bin = <<>>,

	[
		{"Packing Payload will give Bin",
			?_assertEqual(Bin,smpp34pdu_unbind_resp:pack(Payload))},
		{"Unpacking Bin will give Payload",
			?_assertEqual(Payload, smpp34pdu_unbind_resp:unpack(Bin))},
		{"Packing and Unpacking Payload will give you Payload", 
			?_assertEqual(Payload,
					smpp34pdu_unbind_resp:unpack(smpp34pdu_unbind_resp:pack(Payload)))},
		{"Unpacking and Packing Bin will give you Bin", 
			?_assertEqual(Bin,
					smpp34pdu_unbind_resp:pack(smpp34pdu_unbind_resp:unpack(Bin)))}
	].
