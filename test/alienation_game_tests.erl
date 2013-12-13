-module(alienation_game_tests).
-include_lib("eunit/include/eunit.hrl").

add_player_test() ->
  State = alienation_game:init(),
  P1 = alienation_game:add_player(State, "luiz"),
  ?assertEqual(1, length(alienation_game:players(P1))).

game_start_helper() ->
  State = alienation_game:init(),
  P1 = alienation_game:add_player(State, "luiz"),
  P2 = alienation_game:add_player(P1, "larissa"),
  alienation_game:start(normal, P2).

game_start_set_difficulty_level_test() ->
  State = game_start_helper(),
  ?assertEqual(normal, alienation_game:difficulty(State)).

game_start_draw_initial_hands_test() ->
  State = game_start_helper(),
  [Player|_P2] = alienation_game:players(State),
  ?assertEqual(4, length(player:hand(Player))).

game_start_remove_card_from_players_deck_test() ->
  State = game_start_helper(),
  PlayersDeck = alienation_game:players_deck(State),
  ?assertEqual(40, length(PlayersDeck)).

