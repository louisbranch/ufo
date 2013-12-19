-module(alienation_game_tests).
-include_lib("eunit/include/eunit.hrl").

add_player_test() ->
  State = alienation_game:init(),
  P1 = alienation_game:add_player(State, luiz),
  ?assertEqual(1, length(alienation_game:players(P1))).

game_start_test_() ->
  InitState = alienation_game:init(),
  P1 = alienation_game:add_player(InitState, luiz),
  P2 = alienation_game:add_player(P1, larissa),
  State = alienation_game:start(normal, P2),
  [test_set_difficulty_level(State),
   test_draw_initial_hands(State),
   test_remove_players_deck_cards(State),
   test_set_current_player(State)].

test_set_difficulty_level(State) ->
  ?_assertEqual(normal, alienation_game:difficulty(State)).

test_draw_initial_hands(State) ->
  [Player|_P2] = alienation_game:players(State),
  ?_assertEqual(4, length(alienation_player:hand(Player))).

test_remove_players_deck_cards(State) ->
  PlayersDeck = alienation_game:players_deck(State),
  ?_assertEqual(40, length(PlayersDeck)).

test_set_current_player(State) ->
  Player = alienation_game:current_player(State),
  ?_assertEqual(luiz, alienation_player:name(Player)).

