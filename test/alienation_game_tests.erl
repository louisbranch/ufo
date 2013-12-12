-module(alienation_game_tests).
-include_lib("eunit/include/eunit.hrl").

game_start_helper() ->
  State = alienation_game:init("luiz"),
  WithPlayer = alienation_game:add_player(State, "larissa"),
  alienation_game:start(normal, WithPlayer).

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

