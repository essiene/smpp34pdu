-module(smpp34pdu_replace_sm).
-include("smpp34pdu.hrl").
-include("types.hrl").
-export([pack/1,unpack/1]).
-import(pdu_data, [cstring_to_bin/2, string_to_bin/2, integer_to_bin/2]).
-import(pdu_data, [bin_to_cstring/2, bin_to_string/2, bin_to_integer/2]).

-spec(pack/1 :: (replace_sm()) -> binary()).
-spec(unpack/1 :: (binary()) -> replace_sm()).

pack(#replace_sm{message_id=MessageId, 
		source_addr_ton=SrcAddrTon,
		source_addr_npi=SrcAddrNpi,
		source_addr=SrcAddr,
		schedule_delivery_time=SchDeliveryTime,
		validity_period=ValidityPeriod,
		registered_delivery=RegDelivery,
		sm_default_msg_id=SmDefaultMsgId,
		sm_length=SmLength,
		short_message=ShortMessage}) ->


		L = [cstring_to_bin(MessageId, 65),
					   integer_to_bin(SrcAddrTon, 1),
					   integer_to_bin(SrcAddrNpi, 1),
					   cstring_to_bin(SrcAddr, 21),
					   cstring_to_bin(SchDeliveryTime, 17),
					   cstring_to_bin(ValidityPeriod, 17),
					   integer_to_bin(RegDelivery, 1),
					   integer_to_bin(SmDefaultMsgId, 1),
					   integer_to_bin(SmLength, 1),
					   string_to_bin(ShortMessage, 254)],

		list_to_binary(L).


unpack(Bin0) ->
	{MessageId, Bin1} = bin_to_cstring(Bin0, 65),
	{SrcAddrTon, Bin2} = bin_to_integer(Bin1, 1),
	{SrcAddrNpi, Bin3} = bin_to_integer(Bin2, 1),
	{SrcAddr, Bin4} = bin_to_cstring(Bin3, 21),
	{SchDeliveryTime, Bin5} = bin_to_cstring(Bin4, 17),
	{ValidityPeriod, Bin6} = bin_to_cstring(Bin5, 17),
	{RegDelivery, Bin7} = bin_to_integer(Bin6, 1),
	{SmDefaultMsgId, Bin8} = bin_to_integer(Bin7, 1),
	{SmLength, Bin9} = bin_to_integer(Bin8, 1),
	{ShortMessage, <<>>} = bin_to_string(Bin9, 254),


	#replace_sm{message_id=MessageId, 
		source_addr_ton=SrcAddrTon,
		source_addr_npi=SrcAddrNpi,
		source_addr=SrcAddr,
		schedule_delivery_time=SchDeliveryTime,
		validity_period=ValidityPeriod,
		registered_delivery=RegDelivery,
		sm_default_msg_id=SmDefaultMsgId,
		sm_length=SmLength,
		short_message=ShortMessage}.
