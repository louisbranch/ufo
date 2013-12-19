-module(alienation_city_tests).
-include_lib("eunit/include/eunit.hrl").

new_city_test() ->
  ?assertEqual({city, rio, blue, [], [], 0}, alienation_city:new(rio, blue)).

find_non_existing_city_test() ->
  ?assertExit(city_not_found, alienation_city:find(rio, [])).

find_existing_city_test() ->
  City1 = alienation_city:new(sp, red),
  City2 = alienation_city:new(rio, blue),
  ?assertEqual(City2, alienation_city:find(rio, [City1, City2])).

aliens_getter_test() ->
  City = alienation_city:new(rio, blue),
  ?assertEqual([], alienation_city:aliens(City)).

aliens_setter_test() ->
  City = alienation_city:new(rio, blue),
  ?assertEqual({city, rio, blue, [], [1], 0}, alienation_city:aliens(City, [1])).

connections_getter_test() ->
  City = alienation_city:new(sp, red, [rio]),
  ?assertEqual([rio], alienation_city:connections(City)).
