-module(pdu_data_tests).
-include_lib("eunit/include/eunit.hrl").

string_to_bin_test_() ->
	[
		{"A string less than MAX will convert", 
			?_assert(<<"foo!">> == pdu_data:string_to_bin("foo!", 4))},
		{"A string less than max will pass but truncated",
			?_assert(<<"fo">> == pdu_data:string_to_bin("foo!", 2))},
		{"No function clause for non strings",
    		?_assertError(function_clause, pdu_data:string_to_bin(1, 4))},
		{"The atom 'undefined' will yield an empty binary",
    		?_assert(<<>> == pdu_data:string_to_bin(undefined, 8))},
		{"An empty string will yield empty binary",
			?_assert(<<>> == pdu_data:string_to_bin("", 8))}
	].

bin_to_string_test_() ->
	[
		{"Binary less than MAX will unpack with empty binary left", 
			?_assertEqual({"foo", <<>>}, pdu_data:bin_to_string(<<102,111,111>>, 4))},
		{"Binary greater than MAX will unpack with extra binary left",
			?_assertEqual({"foo", <<1,2,3>>}, pdu_data:bin_to_string(<<102,111,111,1,2,3>>, 3))},
		{"Empty binary will unpack to empty string",
			?_assertEqual({"", <<>>}, pdu_data:bin_to_string(<<>>, 3))}
	].

cstring_to_bin_test_() ->
	[
		{"A string less than MAX will convert", 
			?_assert(<<102,111,111,0>> == pdu_data:cstring_to_bin("foo", 4))},
		{"A string less than max will pass but truncated",
			?_assert(<<102,0>> == pdu_data:cstring_to_bin("foo!", 2))},
		{"No function clause for non strings",
    		?_assertError(function_clause, pdu_data:cstring_to_bin(1, 4))},
		{"The atom 'undefined' will yield null terminated, empty binary",
    		?_assert(<<0>> == pdu_data:cstring_to_bin(undefined, 8))},
		{"An empty string will yield null terminated, empty binary",
			?_assert(<<0>> == pdu_data:cstring_to_bin("", 8))}
	].


integer_to_bin_test_() ->
	[
		{"Size specification are octets", 
			[{"One octet when size is 1", 
				?_assert(<<32>> == pdu_data:integer_to_bin(32, 1))},
			 {"Two octets when size is 2, payload is in LSB", 
				?_assert(<<0,32>> == pdu_data:integer_to_bin(32, 2))},
			 {"Three octets when size is 3, payload is in LSB", 
				?_assert(<<0,0,32>> == pdu_data:integer_to_bin(32, 3))},
			 {"Four octets when size is 4, payload is in LSB", 
				?_assert(<<0,0,0,32>> == pdu_data:integer_to_bin(32, 4))}
			]
		},

		{"No function clause for non integers", 
			?_assertError(function_clause, pdu_data:integer_to_bin(1.0, 4))},

		{"The atom 'undefined' will be ZERO, with proper octet size",
			[
				{"Size of 1 should yield one octet", 
					?_assert(<<0>> == pdu_data:integer_to_bin(undefined, 1))},
				{"Size of 2 should yield two octets", 
					?_assert(<<0,0>> == pdu_data:integer_to_bin(undefined, 2))},
				{"Size of 3 should yield three octets", 
					?_assert(<<0,0,0>> == pdu_data:integer_to_bin(undefined, 3))},
				{"Size of 4 should yield four octets", 
					?_assert(<<0,0,0,0>> == pdu_data:integer_to_bin(undefined, 4))}
			]
		},

		{"The integer ZERO will be ZERO, with proper octet size",
			[
				{"Size of 1 should yield one octet", 
					?_assert(<<0>> == pdu_data:integer_to_bin(0, 1))},
				{"Size of 2 should yield two octets", 
					?_assert(<<0,0>> == pdu_data:integer_to_bin(0, 2))},
				{"Size of 3 should yield three octets", 
					?_assert(<<0,0,0>> == pdu_data:integer_to_bin(0, 3))},
				{"Size of 4 should yield four octets", 
					?_assert(<<0,0,0,0>> == pdu_data:integer_to_bin(0, 4))}
			]
		}
	].


