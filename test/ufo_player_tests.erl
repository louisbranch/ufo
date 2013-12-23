-module(ufo_player_tests).
-include_lib("eunit/include/eunit.hrl").

players_helper() ->
  P1 = ufo_player:add([], luiz),
  ufo_player:add(P1, larissa).

new_player_test() ->
  ?assertEqual({player, luiz, []}, ufo_player:new(luiz)).

add_player_test() ->
  ?assertEqual([{player, larissa, []}, {player, luiz, []}], players_helper()).

get_name_test() ->
  Player = ufo_player:new(luiz),
  ?assertEqual(luiz, ufo_player:name(Player)).

get_hand_test() ->
  Player = ufo_player:new(luiz),
  ?assertEqual([], ufo_player:hand(Player)).

set_hand_test() ->
  Player = ufo_player:new(luiz),
  PlayerWithHand = ufo_player:hand(Player, [1,2,3]),
  ?assertEqual([1,2,3], ufo_player:hand(PlayerWithHand)).

distribute_hands_test() ->
  Hands = [[{rio, blue}], [{ny, red}]],
  ?assertEqual(
     [{player, luiz, [{ny, red}]},
      {player, larissa, [{rio, blue}]}],
    ufo_player:distribute_hands(players_helper(), Hands)
  ).
