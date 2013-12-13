-module(alienation_player_tests).
-include_lib("eunit/include/eunit.hrl").

players_helper() ->
  P1 = alienation_player:add([], luiz),
  alienation_player:add(P1, larissa).

new_player_test() ->
  ?assertEqual({player, luiz, []}, alienation_player:new(luiz)).

add_player_test() ->
  ?assertEqual([{player, larissa, []}, {player, luiz, []}], players_helper()).

remove_player_test() ->
  ?assertEqual([{player, larissa, []}], alienation_player:remove(players_helper(), luiz)).

get_name_test() ->
  Player = alienation_player:new(luiz),
  ?assertEqual(luiz, alienation_player:name(Player)).

get_hand_test() ->
  Player = alienation_player:new(luiz),
  ?assertEqual([], alienation_player:hand(Player)).

set_hand_test() ->
  Player = alienation_player:new(luiz),
  PlayerWithHand = alienation_player:hand(Player, [1,2,3]),
  ?assertEqual([1,2,3], alienation_player:hand(PlayerWithHand)).

distribute_hands_test() ->
  Hands = [[{rio, blue}], [{ny, red}]],
  ?assertEqual(
     [{player, luiz, [{ny, red}]},
      {player, larissa, [{rio, blue}]}],
    alienation_player:distribute_hands(players_helper(), Hands)
  ).
