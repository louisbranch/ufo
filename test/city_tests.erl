-module(city_tests).
-include_lib("eunit/include/eunit.hrl").

new_city_test() ->
  {city, rio, blue, [], 0} = city:new(rio, blue).

find_non_existing_city_test() ->
  ?assertExit(city_not_found, city:find(rio, [])).

find_existing_city_test() ->
  City1 = city:new(sp, red),
  City2 = city:new(rio, blue),
  ?assertEqual(City2, city:find(rio, [City1, City2])).

connections_test() ->
  Connections = [{ny,rio},{sp,ny},{rio,sp}],
  City1 = city:new(sp, red),
  City2 = city:new(ny, green),
  City3 = city:new(rio, blue),
  Map = [City1, City2, City3],
  ?assertEqual([City2, City1], city:connections(City3, Connections, Map)).
