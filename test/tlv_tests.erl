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
			 {"additional_status_info_text", 
				?_assertEqual(<<0,29,0,8,102,111,111,32,98,97,114,0>>, tlv:pack(?ADDITIONAL_STATUS_INFO_TEXT, "foo bar"))},
			 {"receipted_message_id", 
				?_assertEqual(<<0,30,0,7,102,111,111,98,97,114,0>>, tlv:pack(?RECEIPTED_MESSAGE_ID, "foobar"))},
			 {"ms_msg_wait_facilities", 
				?_assertEqual(<<0,48,0,1,130>>, tlv:pack(?MS_MSG_WAIT_FACILITIES, ?MSG_WAIT_ACTIVE bor ?MSG_WAIT_TYPE_EMAIL))},
			 {"privacy_indicator", 
				?_assertEqual(<<2,1,0,1,3>>, tlv:pack(?PRIVACY_INDICATOR, ?PRIVACY_INDICATOR_SECRET))},
			 {"addtional_status_info_text", 
				?_assertEqual(<<0,29,0,8,102,111,111,32,98,97,114,0>>, tlv:pack(?ADDITIONAL_STATUS_INFO_TEXT, "foo bar"))},
			 {"receipted_message_id", 
				?_assertEqual(<<0,30,0,7,102,111,111,98,97,114,0>>, tlv:pack(?RECEIPTED_MESSAGE_ID, "foobar"))},
			 {"source_subaddress", 
				?_assertEqual(<<2,2,0,5,1,2,3,4,5>>, tlv:pack(?SOURCE_SUBADDRESS, <<1,2,3,4,5>>))},
			 {"dest_subaddress", 
				?_assertEqual(<<2,3,0,5,1,2,3,4,5>>, tlv:pack(?DEST_SUBADDRESS, <<1,2,3,4,5>>))},
			 {"user_message_reference", 
				?_assertEqual(<<2,4,0,2,1,4>>, tlv:pack(?USER_MESSAGE_REFERENCE, 260))},
			 {"user_response_code", 
				?_assertEqual(<<2,5,0,1,67>>, tlv:pack(?USER_RESPONSE_CODE, 67))},
			 {"language_indicator", 
				?_assertEqual(<<2,13,0,1,2>>, tlv:pack(?LANGUAGE_INDICATOR, ?LANGUAGE_INDICATOR_FRENCH))},
			 {"source_port", 
				?_assertEqual(<<2,10,0,2,1,59>>, tlv:pack(?SOURCE_PORT, 315))},
			 {"destination_port", 
				?_assertEqual(<<2,11,0,2,2,26>>, tlv:pack(?DESTINATION_PORT, 538))},
			 {"sar_msg_ref_num", 
				?_assertEqual(<<2,12,0,2,0,111>>, tlv:pack(?SAR_MSG_REF_NUM, 111))},
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
				{"sc_interface_version", 
					?_assertEqual({16#34, <<>>}, tlv:unpack(?SC_INTERFACE_VERSION, <<0,1,52>>))}
			]
		}
	].
