-module(smpp34_pdu).
-include("pdu.hrl").
-include("types.hrl").
-export([pack/2, unpack/1]).

-type(unpack_cont_action() :: 'header_length' | 'body_length' | 'next').

-spec(pack/2 :: (integer(), bind_receiver()) -> binary()).
-spec(pack/4 :: (integer(), integer(), integer(), binary()) -> binary()).

-spec(unpack/1 :: (binary()) -> {[pdu()],binary()}).
-spec(unpack/3 :: (binary(), unpack_cont_action(), [pdu()]) -> {[pdu()], binary()}).

-spec(unpack_body/2 :: (integer(), binary()) -> bind_receiver() | invalid_command_id()).


pack(Snum, #bind_receiver{}=Body) ->
	Bin = smpp34_BIND_RECEIVER:pack(Body),
	pack(?BIND_RECEIVER, 0, Snum, Bin).


pack(Cid, Cstat, Snum, Body) ->
		Clen = byte_size(Body) + ?HEADER_OCTET_SIZE,
		L = [<<Clen:32,Cid:32,Cstat:32,Snum:32>>, Body],
		list_to_binary(L).



unpack(Bin) ->
	unpack(Bin, next, []).


unpack(Bin, header_length, Accm) ->
	{lists:reverse(Accm), Bin};
unpack(Bin, body_length, Accm) ->
	{lists:reverse(Accm), Bin};
unpack(Bin0, next, Accm) ->
	case byte_size(Bin0) of
		N when N < 16 ->
			unpack(Bin0, header_length, Accm);
		_ ->
			{CommandLength, Bin1} = pdu_data:bin_to_integer(Bin0, 4),
			{CommandId, Bin2} = pdu_data:bin_to_integer(Bin1, 4),
			{CommandStatus, Bin3} = pdu_data:bin_to_integer(Bin2, 4),
			{SequenceNumber, Bin4} = pdu_data:bin_to_integer(Bin3, 4),

			case byte_size(Bin4) of
				N when N < CommandLength ->
					unpack(Bin4, body_length, Accm);
				_ ->
					<<BodyBin:CommandLength/binary, Rest/binary>> = Bin4,
					Body = unpack_body(CommandId, BodyBin),
					Pdu = #pdu{command_length=CommandLength,
						       command_id=CommandId,
							   command_status=CommandStatus,
							   sequence_number=SequenceNumber,
							   body=Body},
					unpack(Rest, next, [Pdu|Accm])
			end
	end.

unpack_body(?BIND_RECEIVER, Bin) ->
	smpp34_BIND_RECEIVER:unpack(Bin);
unpack_body(CommandId, _) ->
	{error, {command_id, CommandId}}.
