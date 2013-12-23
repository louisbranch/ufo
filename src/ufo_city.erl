-module(ufo_city).
-export([new/2, new/3, find/2, aliens/1, aliens/2, connections/1, name/1,
         players/1, players/2, initial_position/2, move/4, fly/4]).
-record(city, {
        name::atom(),
        type::atom(),
        connections=[]::[atom()],
        aliens=[]::[tuple()],
        players=[]::[ufo_player:player()],
        hqs=false::boolean()
        }).
-opaque city() :: #city{}.
-export_type([city/0]).

%% @doc Create a city with given name and type
-spec new(atom(), atom()) -> city().
new(Name, Type) -> new(Name, Type, []).

%% @doc Create a city with given name, type and other cities connections
-spec new(atom(), atom(), [atom()]) -> city().
new(Name, Type, Connections) ->
    #city{name=Name, type=Type, connections=Connections}.

%% @doc Get city aliens
-spec aliens(city()) -> [tuple()].
aliens(City) -> City#city.aliens.

%% @doc Set city aliens
-spec aliens(city(), [tuple()]) -> city().
aliens(City, Aliens) -> City#city{aliens=Aliens}.

%% @doc Get city connections
-spec connections(city()) -> [atom()].
connections(City) -> City#city.connections.

%% @doc Find city on map given its name
%% @throws 'city_not_found'
-spec find(atom(), [city()]) -> city().
find(Name, Map) ->
    case lists:keyfind(Name, #city.name, Map) of
        false -> throw(city_not_found);
        City -> City
    end.

%% @doc Get city name
-spec name(city()) -> atom().
name(City) -> City#city.name.

%% @doc Get city players
-spec players(city()) -> [ufo_player:player()].
players(City) -> City#city.players.

%% @doc Set city players
-spec players(city(), [ufo_player:player()]) -> city().
players(City, Players) -> City#city{players=Players}.

%% @doc Set initial position for all players to Atlanta
-spec initial_position([city()], [ufo_player:player()]) -> [city()].
initial_position(Map, Players) ->
    City = find(atlanta, Map),
    NewCity = players(City, Players),
    update(NewCity, Map).

%% @doc Move player to adjacent city
-spec move(ufo_player:player(), atom(), atom(), [city()]) ->
    {'ok', [city()]}
    | {'error', 'wrong_origin'}
    | {'error', 'not_adjecent'}.
move(Player, NameOrigin, NameDestiny, Map) ->
    Origin = find(NameOrigin, Map),
    Destiny = find(NameDestiny, Map),
    case adjecent(Origin, Destiny) of
        true -> realocate_player(Player, Origin, Destiny, Map);
        false -> {error, not_adjecent}
    end.

%% @doc Move player to any other city
-spec fly(ufo_player:player(), atom(), atom(), [city()]) ->
    {'ok', [city()]}
    | {'error', 'wrong_origin'}.
fly(Player, NameOrigin, NameDestiny, Map) ->
    Origin = find(NameOrigin, Map),
    Destiny = find(NameDestiny, Map),
    realocate_player(Player, Origin, Destiny, Map).

%% @doc Remove a player from a city and add to another
-spec realocate_player(ufo_player:player(), city(), city(), [city()]) ->
    {'ok', [city()]} | {'error', 'wrong_origin'}.
realocate_player(Player, Origin, Destiny, Map) ->
    Players = lists:delete(Player, players(Origin)),
    NewOrigin = players(Origin, Players),
    if
        Origin =:= NewOrigin -> {error, wrong_origin};
        true ->
            NewDestiny = players(Destiny, [Player|players(Destiny)]),
            NewMap = update(NewOrigin, Map),
            {ok, update(NewDestiny, NewMap)}
    end.

%% @doc Return whether two cities are connected
-spec adjecent(city(), city()) -> boolean().
adjecent(Origin, Destiny) ->
    Name = name(Destiny),
    lists:any(fun (City) -> City =:= Name end, connections(Origin)).

%% @doc Update a city on map
-spec update(city(), [city()]) -> [city()].
update(City, Map) ->
    lists:keystore(name(City), #city.name, Map, City).
