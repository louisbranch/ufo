-module(player).
-export([new/1, hand/1, hand/2, distribute_hands/2]).
-record(player, {name, hand=[]}).

new(Name) ->
  #player{name=Name}.

distribute_hands(Players, Hands) ->
  {PlayersWithHands, _} = lists:foldl(fun (Player, {Ps, [Hand|Hs]}) ->
    {[hand(Player,Hand)|Ps], Hs}
    end, {[], Hands}, Players),
  lists:reverse(PlayersWithHands).

hand(Player) ->
  Player#player.hand.

hand(Player, Hand) ->
  Player#player{hand=Hand}.
