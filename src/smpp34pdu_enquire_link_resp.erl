-module(smpp34pdu_enquire_link_resp).
-include("pdu.hrl").
-include("types.hrl").
-export([pack/1,unpack/1]).

-spec(pack/1 :: (enquire_link_resp()) -> binary()).
-spec(unpack/1 :: (binary()) -> enquire_link_resp()).

pack(#enquire_link_resp{}) -> <<>>.
unpack(_) -> #enquire_link_resp {}.
