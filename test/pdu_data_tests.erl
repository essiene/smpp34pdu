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


