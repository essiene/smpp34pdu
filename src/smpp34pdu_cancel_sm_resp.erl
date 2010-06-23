-module(smpp34pdu_cancel_sm_resp).
-include("pdu.hrl").
-include("types.hrl").
-export([pack/1,unpack/1]).

-spec(pack/1 :: (cancel_sm_resp()) -> binary()).
-spec(unpack/1 :: (binary()) -> cancel_sm_resp()).

pack(#cancel_sm_resp{}) -> <<>>.
unpack(_) -> #cancel_sm_resp{}.
