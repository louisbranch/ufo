-module(game_server_tests).
-include_lib("eunit/include/eunit.hrl").

game_start_helper() ->
  {ok, Pid} = game_server:start_link("luiz"),
  game_server:start_game(Pid, normal),
  Pid ! {state, self()},
  wait_for_it().

wait_for_it() ->
  receive
    {ok, State} -> State;
    _ -> wait_for_it()
  after 5000 ->
      timeout
  end.

game_start_change_status_test() ->
  State = game_start_helper(),
  ?assertEqual(running, game_server:status(State)).

game_start_set_difficulty_level_test() ->
  State = game_start_helper(),
  ?assertEqual(normal, game_server:difficulty(State)).

game_start_draw_initial_hands_test() ->
  State = game_start_helper(),
  [Player] = game_server:players(State),
  ?assertEqual(5, length(player:hand(Player))).

game_start_remove_card_from_players_deck_test() ->
  State = game_start_helper(),
  PlayersDeck = game_server:players_deck(State),
  ?assertEqual(43, length(PlayersDeck)).

