-module(map_tests).
-include_lib("eunit/include/eunit.hrl").

create_cities_map_test() ->
  Cities = map:cities(),
  ?assertEqual(length(Cities), 48).

create_connections_map_test() ->
  Connections = map:connections(),
  ?assertEqual(length(Connections), 94).

