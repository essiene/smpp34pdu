-module(smpp34pdu_bind_transceiver_resp_tests).
-include("../include/smpp34pdu.hrl").
-include_lib("eunit/include/eunit.hrl").


bind_transceiver_resp_test_() -> 
    Payload = #bind_transceiver_resp{system_id="abcdefghij", sc_interface_version=?VERSION},

    Bin = <<97,98,99,100,101,102,103,104,105,106,0,2,16,0,1,52>>,

	[
		{"Packing Payload will give Bin",
			?_assertEqual(Bin,smpp34pdu_bind_transceiver_resp:pack(Payload))},
		{"Unpacking Bin will give Payload",
			?_assertEqual(Payload, smpp34pdu_bind_transceiver_resp:unpack(Bin))},
		{"Packing and Unpacking Payload will give you Payload", 
			?_assertEqual(Payload,
					smpp34pdu_bind_transceiver_resp:unpack(smpp34pdu_bind_transceiver_resp:pack(Payload)))},
		{"Unpacking and Packing Bin will give you Bin", 
			?_assertEqual(Bin,
					smpp34pdu_bind_transceiver_resp:pack(smpp34pdu_bind_transceiver_resp:unpack(Bin)))}
	].
