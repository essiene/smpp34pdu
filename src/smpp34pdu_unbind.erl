-module(smpp34pdu_unbind).
-include("pdu.hrl").
-include("types.hrl").
-export([pack/1,unpack/1]).

-spec(pack/1 :: (unbind()) -> binary()).
-spec(unpack/1 :: (binary()) -> unbind()).

pack(#unbind{}) -> <<>>.
unpack(_) -> #unbind {}.
