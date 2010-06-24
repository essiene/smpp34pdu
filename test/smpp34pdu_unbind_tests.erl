-module(smpp34pdu_unbind_tests).
-include("../include/pdu.hrl").
-include_lib("eunit/include/eunit.hrl").


unbind_test_() -> 
    Payload = #unbind{},
    Bin = <<>>,

	[
		{"Packing Payload will give Bin",
			?_assertEqual(Bin,smpp34pdu_unbind:pack(Payload))},
		{"Unpacking Bin will give Payload",
			?_assertEqual(Payload, smpp34pdu_unbind:unpack(Bin))},
		{"Packing and Unpacking Payload will give you Payload", 
			?_assertEqual(Payload,
					smpp34pdu_unbind:unpack(smpp34pdu_unbind:pack(Payload)))},
		{"Unpacking and Packing Bin will give you Bin", 
			?_assertEqual(Bin,
					smpp34pdu_unbind:pack(smpp34pdu_unbind:unpack(Bin)))}
	].
