-module(smpp34pdu_alert_notification_tests).
-include("../include/smpp34pdu.hrl").
-include_lib("eunit/include/eunit.hrl").


alert_notification_test_() -> 
    Payload = #alert_notification{source_addr_ton=2,
		source_addr_npi=1,
        source_addr="abcd",
		esme_addr_ton=2,
		esme_addr_npi=1,
		esme_addr="efgh",
		ms_availability_status=?MS_AVAILABILITY_UNAVAILABLE},

    Bin = <<2,1,97,98,99,100,0,2,1,101,102,103,104,0,4,34,0,1,2>>,

    PayloadNoTlv = #alert_notification{source_addr_ton=2,
		source_addr_npi=1,
        source_addr="abcd",
		esme_addr_ton=2,
		esme_addr_npi=1,
		esme_addr="efgh"},

    BinNoTlv = <<2,1,97,98,99,100,0,2,1,101,102,103,104,0>>,

	[
		{"Packing Payload will give Bin",
			?_assertEqual(Bin,smpp34pdu_alert_notification:pack(Payload))},
		{"Unpacking Bin will give Payload",
			?_assertEqual(Payload, smpp34pdu_alert_notification:unpack(Bin))},
		{"Packing PayloadNoTlv will give BinNoTlv",
			?_assertEqual(BinNoTlv,smpp34pdu_alert_notification:pack(PayloadNoTlv))},
		{"Unpacking BinNoTlv will give PayloadNoTlv",
			?_assertEqual(PayloadNoTlv, smpp34pdu_alert_notification:unpack(BinNoTlv))}
	].
