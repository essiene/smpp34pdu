-module(smpp34pdu_outbind).
-include("pdu.hrl").
-include("types.hrl").
-export([pack/1,unpack/1]).
-import(pdu_data, [cstring_to_bin/2, integer_to_bin/2]).
-import(pdu_data, [bin_to_cstring/2, bin_to_integer/2]).

-spec(pack/1 :: (outbind()) -> binary()).
-spec(unpack/1 :: (binary()) -> outbind()).

pack(#outbind{system_id=SystemId, 
		password=Password}) ->

		L = [cstring_to_bin(SystemId, 16),
					   cstring_to_bin(Password, 9)],

		list_to_binary(L).


unpack(Bin0) ->
	{SystemId, Bin1} = bin_to_cstring(Bin0, 16),
	{Password, <<>>} = bin_to_cstring(Bin1, 9),

	#outbind {system_id=SystemId,
		password=Password}.

