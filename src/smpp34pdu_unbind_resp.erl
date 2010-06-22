-module(smpp34pdu_unbind_resp).
-include("pdu.hrl").
-include("types.hrl").
-export([pack/1,unpack/1]).

-spec(pack/1 :: (unbind_resp()) -> binary()).
-spec(unpack/1 :: (binary()) -> unbind_resp()).

pack(#unbind_resp{}) -> <<>>.
unpack(_) -> #unbind_resp{}.
