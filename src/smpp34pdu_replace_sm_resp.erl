-module(smpp34pdu_replace_sm_resp).
-include("pdu.hrl").
-include("types.hrl").
-export([pack/1,unpack/1]).

-spec(pack/1 :: (replace_sm_resp()) -> binary()).
-spec(unpack/1 :: (binary()) -> replace_sm_resp()).

pack(#replace_sm_resp{}) -> <<>>.
unpack(_) -> #replace_sm_resp{}.
