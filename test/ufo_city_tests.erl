-module(ufo_city_tests).
-include_lib("eunit/include/eunit.hrl").

new_city_test() ->
    ?assertEqual({city, rio, blue, [], [], [], false}, ufo_city:new(rio, blue)).

find_non_existing_city_test() ->
    ?assertThrow(city_not_found, ufo_city:find(rio, [])).

find_existing_city_test() ->
    City1 = ufo_city:new(sp, red),
    City2 = ufo_city:new(rio, blue),
    ?assertEqual(City2, ufo_city:find(rio, [City1, City2])).

aliens_test() ->
    City = ufo_city:new(rio, blue),
    CityWithAliens = ufo_city:aliens(City, [1]),
    ?assertEqual([1], ufo_city:aliens(CityWithAliens)).

connections_getter_test() ->
    City = ufo_city:new(sp, red, [rio]),
    ?assertEqual([rio], ufo_city:connections(City)).

name_getter_test() ->
    City = ufo_city:new(sp, red),
    ?assertEqual(sp, ufo_city:name(City)).

players_test() ->
    City = ufo_city:new(rio, blue),
    CityWithPlayers = ufo_city:players(City, [1]),
    ?assertEqual([1], ufo_city:players(CityWithPlayers)).

initial_position_test() ->
    Map = ufo_map:cities(),
    Players = [1,2,3],
    NewMap = ufo_city:initial_position(Map, Players),
    Atlanta = ufo_city:find(atlanta, NewMap),
    ?assertEqual([1,2,3], ufo_city:players(Atlanta)).

move_from_not_adjecent_test() ->
    Map = ufo_map:cities(),
    Player = ufo_player:new(luiz),
    Movement = ufo_city:move(Player, atlanta, montreal, Map),
    ?assertEqual({error, not_adjecent}, Movement).

move_from_wrong_origin_test() ->
    Map = ufo_map:cities(),
    Player = ufo_player:new(luiz),
    Movement = ufo_city:move(Player, chicago, montreal, Map),
    ?assertEqual({error, wrong_origin}, Movement).

move_from_adjecente_cities_test_() ->
    Map = ufo_map:cities(),
    Player = ufo_player:new(luiz),
    InitialMap = ufo_city:initial_position(Map, [Player]),
    Movement = ufo_city:move(Player, atlanta, chicago, InitialMap),
    {ok, NewMap} = Movement,
    [origin_city_test(atlanta, NewMap), destinty_city_test(chicago, NewMap, Player)].

origin_city_test(Name, Map) ->
    City = ufo_city:find(Name, Map),
    ?_assertEqual(1, length(ufo_city:players(City))).

destinty_city_test(Name, Map, Player) ->
    City = ufo_city:find(Name, Map),
    ?_assertEqual([Player], ufo_city:players(City)).
