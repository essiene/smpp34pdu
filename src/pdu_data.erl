-module(pdu_data).
-export([string_to_bin/2, cstring_to_bin/2, integer_to_bin/2]).
-export([bin_to_string/2, bin_to_cstring/2, bin_to_integer/2]).

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

bin_to_string(Bin, Max) ->
	bin_to_string(Bin, 1, Max, []).

bin_to_string(<<>>, _, _, Acc) ->
	{lists:reverse(Acc), <<>>};
bin_to_string(Bin, Current, Max, Acc) when Current > Max ->
	{lists:reverse(Acc), Bin};
bin_to_string(<<H,Rest/binary>>, Current, Max, Acc) ->
	bin_to_string(Rest, Current+1, Max, [H|Acc]).


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

bin_to_cstring(Bin, Max) ->
	bin_to_cstring(Bin, 1, Max, []).

bin_to_cstring(<<>>, _,_, Acc) ->
	{lists:reverse(Acc), <<>>};
bin_to_cstring(<<0,Rest/binary>>, _, _, Acc) ->
	{lists:reverse(Acc), Rest};
bin_to_cstring(<<H,Rest/binary>>, Max, Max, Acc) ->
	{lists:reverse([H|Acc]), Rest};
bin_to_cstring(<<H,Rest/binary>>, Current, Max, Acc) ->
	bin_to_cstring(Rest, Current+1, Max, [H|Acc]);
bin_to_cstring(Bin, Current, Max, Acc) when Current > Max ->
	{lists:reverse(Acc), Bin}.


integer_to_bin(undefined, Max) ->
    Size = Max*8,
    <<0:Size>>;
integer_to_bin(Data0, Max) when is_integer(Data0) ->
    Size = Max*8,
    <<Data0:Size>>.

bin_to_integer(Bin, Max) ->
    Size = Max * 8,
    <<Integer:Size, Rest/binary>> = Bin,
    {Integer, Rest}.
