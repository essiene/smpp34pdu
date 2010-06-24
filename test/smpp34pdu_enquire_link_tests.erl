-module(smpp34pdu_enquire_link_tests).
-include("../include/pdu.hrl").
-include_lib("eunit/include/eunit.hrl").


enquire_link_test_() -> 
    Payload = #enquire_link{},
    Bin = <<>>,

	[
		{"Packing Payload will give Bin",
			?_assertEqual(Bin,smpp34pdu_enquire_link:pack(Payload))},
		{"Unpacking Bin will give Payload",
			?_assertEqual(Payload, smpp34pdu_enquire_link:unpack(Bin))},
		{"Packing and Unpacking Payload will give you Payload", 
			?_assertEqual(Payload,
					smpp34pdu_enquire_link:unpack(smpp34pdu_enquire_link:pack(Payload)))},
		{"Unpacking and Packing Bin will give you Bin", 
			?_assertEqual(Bin,
					smpp34pdu_enquire_link:pack(smpp34pdu_enquire_link:unpack(Bin)))}
	].
