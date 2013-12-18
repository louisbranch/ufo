-module(alienation_deck_tests).
-include_lib("eunit/include/eunit.hrl").

players_deck_test() ->
  ?assertEqual(48, length(alienation_deck:players())).

shuffle_deck_test() ->
  Deck1 = alienation_deck:players(),
  Deck2 = alienation_deck:players(),
  ?assertNotEqual(Deck1, Deck2).
