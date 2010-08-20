-module(pdu_data_tests).
-include_lib("eunit/include/eunit.hrl").

string_to_bin_test_() ->
	[
		{"A string less than MAX will convert", 
			?_assert(<<"foo">> == pdu_data:string_to_bin("foo", 4))},
		{"A string greater than max will pass but truncated",
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
		{"A string greater than max will pass but truncated",
			?_assert(<<102,0>> == pdu_data:cstring_to_bin("foo!", 2))},
		{"No function clause for non strings",
    		?_assertError(function_clause, pdu_data:cstring_to_bin(1, 4))},
		{"The atom 'undefined' will yield null terminated, empty binary",
    		?_assert(<<0>> == pdu_data:cstring_to_bin(undefined, 8))},
		{"An empty string will yield null terminated, empty binary",
			?_assert(<<0>> == pdu_data:cstring_to_bin("", 8))}
	].

bin_to_cstring_test_() ->
	[
		{"Binary less than MAX will unpack with empty binary left", 
			?_assertEqual({"foo", <<>>}, pdu_data:bin_to_cstring(<<102,111,111,0>>, 6))},
		{"Binary greater than MAX will unpack with extra binary left (basically, lost!)",
			?_assertEqual({"foo", <<109,97,110,99,104,117,0>>}, pdu_data:bin_to_cstring(<<102,111,111,109,97,110,99,104,117,0>>, 3))},
		{"When lenght is zero, empty string is returned with Rest intact",
			?_assertEqual({"", <<109,97,110,99,104,117,0>>}, pdu_data:bin_to_cstring(<<109,97,110,99,104,117,0>>, 0))},
		{"Encountering 0 terminator at start of binary, unpacks empty string and returns rest",
			?_assertEqual({"", <<1,2,3>>}, pdu_data:bin_to_cstring(<<0,1,2,3>>, 8))}
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

bin_to_integer_test_() ->
	[
		{"Unpacking binary of same size as MAX results in proper binary WITH NO extras left",
			?_assert({12345, <<>>} == pdu_data:bin_to_integer(<<0,0,48,57>>, 4))},
		{"Unpacking binary greater than MAX results in proper binary WITH extras left",
			?_assert({12345, <<1,2,3>>} == pdu_data:bin_to_integer(<<0,0,48,57,1,2,3>>, 4))}
	].
		
octstring_to_bin_test_() ->
	[
		{"A bin less than min will return error", 
			?_assertEqual({error, {less_than_min, 3}}, pdu_data:octstring_to_bin(<<0,1>>, {3, 4}))},
		{"A bin greater than min and less than MAX will pass", 
			?_assertEqual(<<0,1,2>>, pdu_data:octstring_to_bin(<<0,1,2>>, {2, 4}))},
		{"A bin greater than MAX will pass but truncated",
			?_assertEqual(<<0,1,2>>, pdu_data:octstring_to_bin(<<0,1,2,3,4>>, {2, 3}))},
		{"A bin equal to MAX will pass exactly",
			?_assertEqual(<<0,1,2,3,4>>, pdu_data:octstring_to_bin(<<0,1,2,3,4>>, {2, 5}))},
		{"A bin less than LEN will return error",
			?_assertEqual({error, {less_than_min, 3}}, pdu_data:octstring_to_bin(<<0,1>>, 3))},
		{"A bin greater than LEN will be truncated",
			?_assertEqual(<<0,1,2>>, pdu_data:octstring_to_bin(<<0,1,2,3,4,5>>, 3))},
		{"No function clause for non binary",
    		?_assertError(function_clause, pdu_data:octstring_to_bin(1, 4))},
		{"The atom 'undefined' will yield empty binary",
    		?_assertEqual(<<>>, pdu_data:octstring_to_bin(undefined, 8))},
		{"An empty bin will yield empty binary",
			?_assertEqual(<<>>, pdu_data:octstring_to_bin(<<>>, 8))}
	].

bin_to_octstring_test_() ->
	[
		{"A bin less than MAX will pass with no remainder", 
			?_assertEqual({<<0,1,2>>, <<>>}, pdu_data:bin_to_octstring(<<0,1,2>>, 4))},
		{"A bin greater than MAX will pass with remainder",
			?_assertEqual({<<0,1,2>>, <<3,4>>}, pdu_data:bin_to_octstring(<<0,1,2,3,4>>, 3))},
		{"A bin equal to MAX will pass exactly with no remainder",
			?_assertEqual({<<0,1,2,3,4>>, <<>>}, pdu_data:bin_to_octstring(<<0,1,2,3,4>>, 5))}
	].

