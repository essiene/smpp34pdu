-module(smpp34pdu_alert_notification).
-include("smpp34pdu.hrl").
-include("types.hrl").
-include("tlv_macros.hrl").

-export([pack/1,unpack/1]).
-import(pdu_data, [cstring_to_bin/2, integer_to_bin/2]).
-import(pdu_data, [bin_to_cstring/2, bin_to_integer/2]).

-spec(pack/1 :: (alert_notification()) -> binary()).
-spec(unpack/1 :: (binary()) -> alert_notification()).

pack(#alert_notification{source_addr_ton=SrcAddrTon,
		source_addr_npi=SrcAddrNpi,
		source_addr=SrcAddr,
		esme_addr_ton=EsmeAddrTon,
		esme_addr_npi=EsmeAddrNpi,
		esme_addr=EsmeAddr,
		ms_availability_status=MsAvailStat}) ->


		L = [integer_to_bin(SrcAddrTon, 1),
					   integer_to_bin(SrcAddrNpi, 1),
					   cstring_to_bin(SrcAddr, 21),
					   integer_to_bin(EsmeAddrTon, 1),
					   integer_to_bin(EsmeAddrNpi, 1),
					   cstring_to_bin(EsmeAddr, 21),
					   tlv:pack(?MS_AVAILABILITY_STATUS, MsAvailStat)],

		list_to_binary(L).


unpack(Bin0) ->
	{SrcAddrTon, Bin1} = bin_to_integer(Bin0, 1),
	{SrcAddrNpi, Bin2} = bin_to_integer(Bin1, 1),
	{SrcAddr, Bin3} = bin_to_cstring(Bin2, 21),
	{EsmeAddrTon, Bin4} = bin_to_integer(Bin3, 1),
	{EsmeAddrNpi, Bin5} = bin_to_integer(Bin4, 1),
	{EsmeAddr, Bin6} = bin_to_cstring(Bin5, 21),

	unpack_tlv_fields(Bin6, #alert_notification {source_addr_ton=SrcAddrTon,
		source_addr_npi=SrcAddrNpi,
		source_addr=SrcAddr,
		esme_addr_ton=EsmeAddrTon,
		esme_addr_npi=EsmeAddrNpi,
		esme_addr=EsmeAddr}).

?TLV_UNPACK_EMPTY_BIN;
?TLV_UNPACK_FIELD(alert_notification, ms_availability_status,?MS_AVAILABILITY_STATUS);
?TLV_UNPACK_UNEXPECTED.


