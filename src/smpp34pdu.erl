-module(smpp34pdu).
-include("pdu.hrl").
-include("types.hrl").
-export([pack/2, unpack/1]).

-type(unpack_status() :: 'header_length' | 'body_length' | 'ok').

-spec(pack/2 :: (integer(), valid_pdu()) -> binary()).
-spec(pack/4 :: (integer(), integer(), integer(), binary()) -> binary()).

-spec(unpack/1 :: (binary()) -> {unpack_status(), [pdu()],binary()}).
-spec(unpack/3 :: (binary(), unpack_status(), [pdu()]) -> {unpack_status(), [pdu()], binary()}).

-spec(unpack_body/2 :: (integer(), binary()) -> valid_pdu() | invalid_command_id()).

pack(Snum, #generic_nack{}) ->
	pack(?GENERIC_NACK, 0, Snum, <<>>);

pack(Snum, #bind_receiver{}=Body) ->
	Bin = smpp34pdu_bind_receiver:pack(Body),
	pack(?BIND_RECEIVER, 0, Snum, Bin);

pack(Snum, #bind_transmitter{}=Body) ->
	Bin = smpp34pdu_bind_transmitter:pack(Body),
	pack(?BIND_TRANSMITTER, 0, Snum, Bin);

pack(Snum, #submit_sm_resp{}=Body) ->
	Bin = smpp34pdu_submit_sm_resp:pack(Body),
	pack(?SUBMIT_SM_RESP, 0, Snum, Bin);

pack(Snum, #deliver_sm_resp{}=Body) ->
	Bin = smpp34pdu_deliver_sm_resp:pack(Body),
	pack(?DELIVER_SM_RESP, 0, Snum, Bin);

pack(Snum, #unbind{}) ->
	pack(?UNBIND, 0, Snum, <<>>);

pack(Snum, #unbind_resp{}) ->
	pack(?UNBIND_RESP, 0, Snum, <<>>);

pack(Snum, #replace_sm_resp{}) ->
	pack(?REPLACE_SM_RESP, 0, Snum, <<>>);

pack(Snum, #cancel_sm_resp{}) ->
	pack(?CANCEL_SM_RESP, 0, Snum, <<>>);

pack(Snum, #bind_transceiver{}=Body) ->
	Bin = smpp34pdu_bind_transceiver:pack(Body),
	pack(?BIND_TRANSCEIVER, 0, Snum, Bin);

pack(Snum, #outbind{}=Body) ->
	Bin = smpp34pdu_outbind:pack(Body),
	pack(?OUTBIND, 0, Snum, Bin);

pack(Snum, #enquire_link{}) ->
	pack(?ENQUIRE_LINK, 0, Snum, <<>>);

pack(Snum, #enquire_link_resp{}) ->
	pack(?ENQUIRE_LINK_RESP, 0, Snum, <<>>).



pack(Cid, Cstat, Snum, Body) ->
		Clen = byte_size(Body) + ?HEADER_OCTET_SIZE,
		L = [<<Clen:32,Cid:32,Cstat:32,Snum:32>>, Body],
		list_to_binary(L).



unpack(Bin) ->
	unpack(Bin, ok, []).


unpack(<<>>, _, Accm) ->
	{ok, lists:reverse(Accm), <<>>};
unpack(Bin, header_length, Accm) ->
	{header_length, lists:reverse(Accm), Bin};
unpack(Bin, body_length, Accm) ->
	{body_length, lists:reverse(Accm), Bin};
unpack(Bin0, ok, Accm) ->
	case byte_size(Bin0) of
		N when N < 16 ->
			unpack(Bin0, header_length, Accm);
		_ ->
			{CommandLength, Bin1} = pdu_data:bin_to_integer(Bin0, 4),
			{CommandId, Bin2} = pdu_data:bin_to_integer(Bin1, 4),
			{CommandStatus, Bin3} = pdu_data:bin_to_integer(Bin2, 4),
			{SequenceNumber, Bin4} = pdu_data:bin_to_integer(Bin3, 4),

			BodyLength = CommandLength - ?HEADER_OCTET_SIZE,

			case byte_size(Bin4) of
				N when N < BodyLength ->
					unpack(Bin4, body_length, Accm);
				_ ->
					<<BodyBin:BodyLength/binary, Rest/binary>> = Bin4,
					Body = unpack_body(CommandId, BodyBin),
					Pdu = #pdu{command_length=CommandLength,
						       command_id=CommandId,
							   command_status=CommandStatus,
							   sequence_number=SequenceNumber,
							   body=Body},
					unpack(Rest, ok, [Pdu|Accm])
			end
	end.

unpack_body(?GENERIC_NACK, _) ->
	#generic_nack{};
unpack_body(?BIND_RECEIVER, Bin) ->
	smpp34pdu_bind_receiver:unpack(Bin);
unpack_body(?BIND_TRANSMITTER, Bin) ->
	smpp34pdu_bind_transmitter:unpack(Bin);
unpack_body(?SUBMIT_SM_RESP, Bin) ->
	smpp34pdu_submit_sm_resp:unpack(Bin);
unpack_body(?DELIVER_SM_RESP, Bin) ->
	smpp34pdu_deliver_sm_resp:unpack(Bin);
unpack_body(?UNBIND, _) ->
	#unbind{};
unpack_body(?UNBIND_RESP, _) ->
	#unbind_resp{};
unpack_body(?REPLACE_SM_RESP, _) ->
	#replace_sm_resp{};
unpack_body(?CANCEL_SM_RESP, _) ->
	#cancel_sm_resp{};
unpack_body(?BIND_TRANSCEIVER, Bin) ->
	smpp34pdu_bind_transceiver:unpack(Bin);
unpack_body(?OUTBIND, Bin) ->
	smpp34pdu_outbind:unpack(Bin);
unpack_body(?ENQUIRE_LINK, _) ->
	#enquire_link{};
unpack_body(?ENQUIRE_LINK_RESP, _) ->
	#enquire_link_resp{};
unpack_body(CommandId, _) ->
	{error, {command_id, CommandId}}.
