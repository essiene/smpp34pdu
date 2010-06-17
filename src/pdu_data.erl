-module(pdu_data).
-export([string_to_bin/2, cstring_to_bin/2, integer_to_bin/2]).

string_to_bin(undefined, _) ->
    <<"">>;
string_to_bin(Data0, Max) when is_list(Data0) ->
    Size = Max,
    Data1 = case length(Data0) of
        N when N =< Size ->
			Data0;
        _ ->
			lists:sublist(Data0, Size)
    end,
	list_to_binary(Data1).

cstring_to_bin(undefined, _) ->
    <<0>>;
cstring_to_bin(Data0, Max) when is_list(Data0) ->
    Size = Max - 1,
    Data1 = case length(Data0) of
        N when N =< Size ->
			Data0;
        _ ->
            lists:sublist(Data0, Size)
    end,
	Data2 = list_to_binary(Data1),
	list_to_binary([Data2, <<0>>]).

integer_to_bin(undefined, Max) ->
    Size = Max*8,
    <<0:Size>>;
integer_to_bin(Data0, Max) when is_integer(Data0) ->
    Size = Max*8,
    <<Data0:Size>>.
