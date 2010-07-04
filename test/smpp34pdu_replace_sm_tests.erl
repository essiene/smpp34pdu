-module(smpp34pdu_replace_sm_tests).
-include("../include/pdu.hrl").
-include_lib("eunit/include/eunit.hrl").


replace_sm_test_() -> 
    Payload = #replace_sm{message_id="abcdefghij",
		source_addr_ton=2,
		source_addr_npi=1,
        source_addr="abcd",
		schedule_delivery_time="100716133059001+",
		validity_period="000014000000000R",
		registered_delivery=?DELIVERY_SMSC_ANY bor ?DELIVERY_SME_NONE bor ?DELIVERY_INTM_ANY,
		sm_default_msg_id=1,
		sm_length=11,
		short_message="hello world"},
		

    Bin = <<97,98,99,100,101,102,103,104,105,106,0,2,
			1,97,98,99,100,0,$1,$0,$0,$7,$1,$6,$1,$3,
			$3,$0,$5,$9,$0,$0,$1,$+,0,$0,$0,$0,$0,$1,
			$4,$0,$0,$0,$0,$0,$0,$0,$0,$0,$R,0,17,1,
			11,104,101,108,108,111,32,119,111,114,108,
			100>>,

	[
		{"Packing Payload will give Bin",
			?_assertEqual(Bin,smpp34pdu_replace_sm:pack(Payload))},
		{"Unpacking Bin will give Payload",
			?_assertEqual(Payload, smpp34pdu_replace_sm:unpack(Bin))},
		{"Packing and Unpacking Payload will give you Payload", 
			?_assertEqual(Payload,
					smpp34pdu_replace_sm:unpack(smpp34pdu_replace_sm:pack(Payload)))},
		{"Unpacking and Packing Bin will give you Bin", 
			?_assertEqual(Bin,
					smpp34pdu_replace_sm:pack(smpp34pdu_replace_sm:unpack(Bin)))}
	].
