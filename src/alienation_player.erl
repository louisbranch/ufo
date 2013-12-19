-module(alienation_player).
-export([new/1, add/2, remove/2, name/1, hand/1, hand/2, distribute_hands/2]).
-record(player, {name::binary(), hand=[]::[tuple()]}).

%% @doc Create new player
-spec new(binary()) -> #player{}.
new(Name) ->
    #player{name=Name}.

%% @doc Create and add player to a players list
-spec add([#player{}], binary()) -> [#player{}].
add(Players, Name) ->
    [new(Name)|Players].

%% @doc Remove player using the name from a players list
-spec remove([#player{}], binary()) -> [#player{}].
remove(Players, Name) ->
    lists:keydelete(Name, #player.name, Players).

%% @doc Get player hand
-spec hand(#player{}) -> [tuple()].
hand(Player) ->
    Player#player.hand.

%% @doc Set player hand
-spec hand(#player{}, [tuple()]) -> #player{}.
hand(Player, Hand) ->
    Player#player{hand=Hand}.

%% @doc Get player name
-spec name(#player{}) -> binary().
name(Player) ->
    Player#player.name.

%% @doc Distribute a hand of cards for each player
-spec distribute_hands([#player{}], [tuple()]) -> [#player{}].
distribute_hands(Players, Hands) ->
    {PlayersWithHands, _} = lists:foldl(fun distribute/2, {[], Hands}, Players),
    PlayersWithHands.

distribute(Player, {Players, [Hand|Hands]}) ->
    {[hand(Player,Hand)|Players], Hands}.
