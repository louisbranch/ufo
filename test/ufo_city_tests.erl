-module(ufo_city_tests).
-include_lib("eunit/include/eunit.hrl").

new_city_test() ->
  ?assertEqual({city, rio, blue, [], [], 0}, ufo_city:new(rio, blue)).

find_non_existing_city_test() ->
  ?assertThrow(city_not_found, ufo_city:find(rio, [])).

find_existing_city_test() ->
  City1 = ufo_city:new(sp, red),
  City2 = ufo_city:new(rio, blue),
  ?assertEqual(City2, ufo_city:find(rio, [City1, City2])).

aliens_getter_test() ->
  City = ufo_city:new(rio, blue),
  ?assertEqual([], ufo_city:aliens(City)).

aliens_setter_test() ->
  City = ufo_city:new(rio, blue),
  ?assertEqual({city, rio, blue, [], [1], 0}, ufo_city:aliens(City, [1])).

connections_getter_test() ->
  City = ufo_city:new(sp, red, [rio]),
  ?assertEqual([rio], ufo_city:connections(City)).
