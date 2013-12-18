-module(alienation_player).
-export([new/1, add/2, remove/2, name/1, hand/1, hand/2, distribute_hands/2]).
-record(player, {name::binary(), hand=[]::[tuple()]}).

-spec new(binary()) -> #player{}.
new(Name) ->
  #player{name=Name}.

-spec add([#player{}], binary()) -> [#player{}].
add(Players, Name) ->
  [new(Name)|Players].

-spec remove([#player{}], binary()) -> [#player{}].
remove(Players, Name) ->
 lists:keydelete(Name, #player.name, Players).

-spec distribute_hands([#player{}], [tuple()]) -> [#player{}].
distribute_hands(Players, Hands) ->
  {PlayersWithHands, _} = lists:foldl(fun (Player, {Ps, [Hand|Hs]}) ->
    {[hand(Player,Hand)|Ps], Hs}
    end, {[], Hands}, Players),
  PlayersWithHands.

-spec hand(#player{}) -> [tuple()].
hand(Player) ->
  Player#player.hand.

-spec hand(#player{}, [tuple()]) -> #player{}.
hand(Player, Hand) ->
  Player#player{hand=Hand}.

%% @doc Return the player name
-spec name(#player{}) -> binary().
name(Player) ->
  Player#player.name.
