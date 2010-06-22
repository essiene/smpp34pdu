-module(smpp34pdu_enquire_link).
-include("pdu.hrl").
-include("types.hrl").
-export([pack/1,unpack/1]).

-spec(pack/1 :: (enquire_link()) -> binary()).
-spec(unpack/1 :: (binary()) -> enquire_link()).

pack(#enquire_link{}) -> <<>>.
unpack(_) -> #enquire_link {}.
