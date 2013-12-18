-module(alienation_player).
-export([new/1, add/2, remove/2, name/1, hand/1, hand/2, distribute_hands/2]).
-record(player, {name::binary(), hand=[]::[tuple()]}).

new(Name) ->
  #player{name=Name}.

add(Players, Name) ->
  [new(Name)|Players].

remove(Players, Name) ->
 lists:keydelete(Name, #player.name, Players).

distribute_hands(Players, Hands) ->
  {PlayersWithHands, _} = lists:foldl(fun (Player, {Ps, [Hand|Hs]}) ->
    {[hand(Player,Hand)|Ps], Hs}
    end, {[], Hands}, Players),
  PlayersWithHands.

hand(Player) ->
  Player#player.hand.

hand(Player, Hand) ->
  Player#player{hand=Hand}.

name(Player) ->
  Player#player.name.
