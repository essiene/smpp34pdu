-ifndef(tlv_macros).
-define(tlv_macros, true).

-define(TLV_UNPACK_EMPTY_BIN(), unpack_tlv_fields(<<>>, Body) -> Body).
-define(TLV_UNPACK_UNEXPECTED(), unpack_tlv_fields(<<Unexpected:?TLV_TAG_SIZE, _/binary>>=Bin, Body) ->
    {_, Rest} = tlv:unpack(Unexpected, Bin),
    unpack_tlv_fields(Rest, Body)).
-define(TLV_UNPACK_FIELD(RECORD_NAME,RECORD_FIELD,FIELD_TAG), unpack_tlv_fields(<<FIELD_TAG:?TLV_TAG_SIZE, _/binary>>=Bin, Body) ->
	{Val, Rest} = tlv:unpack(FIELD_TAG, Bin),
    unpack_tlv_fields(Rest, Body#RECORD_NAME{RECORD_FIELD=Val})).


-endif.
