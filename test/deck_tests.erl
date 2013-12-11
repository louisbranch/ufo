-module(deck_tests).
-include_lib("eunit/include/eunit.hrl").

players_deck_test() ->
  ?assertEqual(48, length(deck:players())).

shuffle_deck_test() ->
  Deck1 = deck:players(),
  Deck2 = deck:players(),
  ?assertNotEqual(Deck1, Deck2).
