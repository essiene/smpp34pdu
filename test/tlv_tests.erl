-module(tlv_tests).
-include("../src/constants.hrl").
-include_lib("eunit/include/eunit.hrl").

tlv_test_() ->
	[
		{"pack", 
			[{"undefined yields empty binary",
				?_assertEqual(<<>>, tlv:pack(?SC_INTERFACE_VERSION, undefined))},
			 {"dest_addr_subunit", 
				?_assertEqual(<<0,5,0,1,0>>, tlv:pack(?DEST_ADDR_SUBUNIT, ?ADDR_SUBUNIT_UNKNOWN))},
			 {"source_addr_subunit", 
				?_assertEqual(<<0,13,0,1,2>>, tlv:pack(?SOURCE_ADDR_SUBUNIT, ?ADDR_SUBUNIT_MOBILE_EQUIPMENT))},
			 {"dest_network_type", 
				?_assertEqual(<<0,6,0,1,1>>, tlv:pack(?DEST_NETWORK_TYPE, ?NETWORK_TYPE_GSM))},
			 {"source_network_type", 
				?_assertEqual(<<0,14,0,1,3>>, tlv:pack(?SOURCE_NETWORK_TYPE, ?NETWORK_TYPE_CDMA))},
			 {"dest_bearer_type", 
				?_assertEqual(<<0,7,0,1,1>>, tlv:pack(?DEST_BEARER_TYPE, ?BEARER_TYPE_SMS))},
			 {"source_bearer_type", 
				?_assertEqual(<<0,15,0,1,4>>, tlv:pack(?SOURCE_BEARER_TYPE, ?BEARER_TYPE_USSD))},
			 {"dest_telematics_id", 
				?_assertEqual(<<0,8,0,2,0,2>>, tlv:pack(?DEST_TELEMATICS_ID, 16#0002))},
			 {"source_telematics_id", 
				?_assertEqual(<<0,16,0,1,1>>, tlv:pack(?SOURCE_TELEMATICS_ID, 16#01))},
			 {"qos_time_to_live", 
				?_assertEqual(<<0,23,0,4,0,0,1,44>>, tlv:pack(?QOS_TIME_TO_LIVE, 300))},
			 {"payload_type", 
				?_assertEqual(<<0,25,0,1,0>>, tlv:pack(?PAYLOAD_TYPE, ?PAYLOAD_TYPE_DEFAULT))},
			 {"sc_interface_version", 
				?_assertEqual(<<2,16,0,1,52>>, tlv:pack(?SC_INTERFACE_VERSION, 16#34))}
			]
		},

		{"unpack",
			[
				{"dest_addr_subunit", 
					?_assertEqual({?ADDR_SUBUNIT_UNKNOWN, <<>>}, tlv:unpack(?DEST_ADDR_SUBUNIT, <<0,1,0>>))},
				{"source_addr_subunit", 
					?_assertEqual({?ADDR_SUBUNIT_MOBILE_EQUIPMENT, <<>>}, tlv:unpack(?SOURCE_ADDR_SUBUNIT, <<0,1,2>>))},
				{"dest_network_type", 
					?_assertEqual({?NETWORK_TYPE_GSM, <<>>}, tlv:unpack(?DEST_NETWORK_TYPE, <<0,1,1>>))},
				{"source_network_type", 
					?_assertEqual({?NETWORK_TYPE_CDMA, <<>>}, tlv:unpack(?SOURCE_NETWORK_TYPE, <<0,1,3>>))},
				{"dest_bearer_type", 
					?_assertEqual({?BEARER_TYPE_SMS, <<>>}, tlv:unpack(?DEST_BEARER_TYPE, <<0,1,1>>))},
				{"source_bearer_type", 
					?_assertEqual({?BEARER_TYPE_USSD, <<>>}, tlv:unpack(?SOURCE_BEARER_TYPE, <<0,1,4>>))},
				{"dest_telematics_id", 
					?_assertEqual({2, <<>>}, tlv:unpack(?DEST_TELEMATICS_ID, <<0,2,0,2>>))},
				{"source_telematics_id", 
					?_assertEqual({1, <<>>}, tlv:unpack(?SOURCE_TELEMATICS_ID, <<0,1,1>>))},
				{"qos_time_to_live", 
					?_assertEqual({300, <<>>}, tlv:unpack(?SOURCE_TELEMATICS_ID, <<0,4,0,0,1,44>>))},
				{"payload_type", 
					?_assertEqual({?PAYLOAD_TYPE_WCMP, <<>>}, tlv:unpack(?PAYLOAD_TYPE, <<0,1,1>>))},
				{"sc_interface_version", 
					?_assertEqual({16#34, <<>>}, tlv:unpack(?SC_INTERFACE_VERSION, <<0,1,52>>))}
			]
		}
	].
