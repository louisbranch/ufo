-module(ufo_deck_tests).
-include_lib("eunit/include/eunit.hrl").

players_deck_test() ->
  ?assertEqual(48, length(ufo_deck:players())).

shuffle_deck_test() ->
  Deck1 = ufo_deck:players(),
  Deck2 = ufo_deck:players(),
  ?assertNotEqual(Deck1, Deck2).
