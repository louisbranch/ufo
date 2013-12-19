-module(alienation_map_tests).
-include_lib("eunit/include/eunit.hrl").

create_cities_map_test() ->
  Cities = alienation_map:cities(),
  ?assertEqual(length(Cities), 48).

city_connections_map_test() ->
  Cities = alienation_map:cities(),
  NewYork = lists:keyfind(new_york, 2, Cities),
  ?assertEqual([montreal, washington, london, madrid],
               erlang:element(4, NewYork)).
