-module(smpp34pdu_query_sm_resp).
-include("smpp34pdu.hrl").
-include("types.hrl").
-export([pack/1,unpack/1]).
-import(pdu_data, [cstring_to_bin/2, integer_to_bin/2]).
-import(pdu_data, [bin_to_cstring/2, bin_to_integer/2]).

-spec(pack/1 :: (query_sm_resp()) -> binary()).
-spec(unpack/1 :: (binary()) -> query_sm_resp()).

pack(#query_sm_resp{message_id=MessageId, 
		final_date=FinalDate,
		message_state=MessageState,
		error_code=ErrCode}) ->

		L = [cstring_to_bin(MessageId, 65),
					   cstring_to_bin(FinalDate, 17),
					   integer_to_bin(MessageState, 1),
					   integer_to_bin(ErrCode, 1)],

		list_to_binary(L).


unpack(Bin0) ->
	{MessageId, Bin1} = bin_to_cstring(Bin0, 65),
	{FinalDate, Bin2} = bin_to_cstring(Bin1, 17),
	{MessageState, Bin3} = bin_to_integer(Bin2, 1),
	{ErrCode, _} = bin_to_integer(Bin3, 1),

	#query_sm_resp {message_id=MessageId,
		final_date=FinalDate,
		message_state=MessageState,
		error_code=ErrCode}.

