-module(tlv_tests).
-include("../src/constants.hrl").
-include_lib("eunit/include/eunit.hrl").

tlv_test_() ->
	[
		{"pack", 
			[{"undefined yields empty binary",
				?_assertEqual(<<>>, tlv:pack(?SC_INTERFACE_VERSION, undefined))},
			 {"sc_interface_version", 
				?_assertEqual(<<2,16,0,1,52>>, tlv:pack(?SC_INTERFACE_VERSION, 16#34))}
			]
		},

		{"unpack",
			[
				{"sc_interface_version", 
					?_assertEqual({16#34, <<>>}, tlv:unpack(?SC_INTERFACE_VERSION, <<0,1,52>>))}
			]
		}
	].
