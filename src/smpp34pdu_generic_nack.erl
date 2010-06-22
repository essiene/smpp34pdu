-module(smpp34pdu_generic_nack).
-include("pdu.hrl").
-include("types.hrl").
-export([pack/1,unpack/1]).

-spec(pack/1 :: (generic_nack()) -> binary()).
-spec(unpack/1 :: (binary()) -> generic_nack()).

pack(#generic_nack{}) -> <<>>.
unpack(_) -> #generic_nack {}.
