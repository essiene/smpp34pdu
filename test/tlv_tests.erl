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
				{"sc_interface_version", 
					?_assertEqual({16#34, <<>>}, tlv:unpack(?SC_INTERFACE_VERSION, <<0,1,52>>))}
			]
		}
	].
