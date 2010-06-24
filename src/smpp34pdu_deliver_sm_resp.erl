-module(smpp34pdu_deliver_sm_resp).
-include("pdu.hrl").
-include("types.hrl").
-export([pack/1,unpack/1]).
-import(pdu_data, [cstring_to_bin/2, integer_to_bin/2]).
-import(pdu_data, [bin_to_cstring/2, bin_to_integer/2]).

-spec(pack/1 :: (deliver_sm_resp()) -> binary()).
-spec(unpack/1 :: (binary()) -> deliver_sm_resp()).

pack(#deliver_sm_resp{message_id=MessageId}) ->
		L = [cstring_to_bin(MessageId, 65)],
		list_to_binary(L).


unpack(Bin0) ->
	{MessageId, <<>>} = bin_to_cstring(Bin0, 65),

	#deliver_sm_resp {message_id=MessageId}.
