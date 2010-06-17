-module(smpp34_BIND_RECEIVER_tests).
-include("../include/pdu.hrl").
-include_lib("eunit/include/eunit.hrl").


bind_receiver_pack_test_() -> 
    Payload = #bind_receiver{system_id="abcdefghij",
        password="abcd",
        addr_ton=2,
        addr_npi=1},

    PduBin = <<0,0,0,37,0,0,0,1,0,0,0,0,0,0,0,1,97,
	           98,99,100,101,102,103,104,105,106,0,
			   97,98,99,100,0,0,52,2,1,0>>,

	Pdu = #pdu {command_id=?BIND_RECEIVER,
			    sequence_number=1,
				body=Payload},
	
	Packed = smpp34_BIND_RECEIVER:pack(Pdu),

	[?_assertEqual(PduBin,Packed)].
