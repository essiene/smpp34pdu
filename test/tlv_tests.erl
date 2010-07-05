-module(tlv_tests).
-include("../src/constants.hrl").
-include_lib("eunit/include/eunit.hrl").

to_bin_test_() ->
	[
		{"to_bin", 
			[{"undefined yields empty binary",
				?_assertEqual(<<>>, tlv:to_bin(?SC_INTERFACE_VERSION, undefined))},
			 {"sc_interface_version", 
				?_assertEqual(<<2,16,0,1,52>>, tlv:to_bin(?SC_INTERFACE_VERSION, 16#34))}
			]
		},

		{"from_bin",
			[
				{"sc_interface_version", 
					?_assertEqual({16#34, <<>>}, tlv:from_bin(?SC_INTERFACE_VERSION, <<0,1,52>>))}
			]
		}
	].
