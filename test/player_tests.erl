-module(player_tests).
-include_lib("eunit/include/eunit.hrl").

new_player_test() ->
  ?assertEqual({player, luiz, []}, player:new(luiz)).

get_hand_test() ->
  Player = player:new(luiz),
  ?assertEqual([], player:hand(Player)).

set_hand_test() ->
  Player = player:new(luiz),
  PlayerWithHand = player:hand(Player, [1,2,3]),
  ?assertEqual([1,2,3], player:hand(PlayerWithHand)).

distribute_hands_test() ->
  Players = [player:new(luiz), player:new(larissa)],
  Hands = [[{rio, blue}], [{ny, red}]],
  ?assertEqual(
     [{player, luiz, [{rio, blue}]},
      {player, larissa, [{ny, red}]}],
     player:distribute_hands(Players, Hands)
  ).
