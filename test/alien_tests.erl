-module(alien_tests).
-include_lib("eunit/include/eunit.hrl").

attack_with_an_invalid_pool_test() ->
  Color = red,
  City = city:new(rio, blue),
  AlienPool = [{blue, 2}],
  ?assertExit(alien_color_not_found, alien:attack(Color, City, AlienPool)).

attack_with_an_invalid_city_aliens_test() ->
  Color = blue,
  City = city:new(rio, blue),
  InvalidCity = city:aliens(City, [{blue, 4}]),
  AlienPool = [{blue, 2}],
  ?assertExit(city_aliens_invalid_number, alien:attack(Color, InvalidCity, AlienPool)).

attack_with_an_empty_pool_test() ->
  Color = red,
  City = city:new(rio, blue),
  AlienPool = [{red, 0}],
  ?assertEqual({game_over, alien_pool_empty}, alien:attack(Color, City, AlienPool)).

attack_a_full_city_test() ->
  Color = blue,
  City = city:new(rio, blue),
  FullCity = city:aliens(City, [{red, 3}]),
  AlienPool = [{blue, 2}],
  ?assertEqual({invasion, FullCity}, alien:attack(Color, FullCity, AlienPool)).

attack_an_attacked_city_test() ->
  Color = red,
  City = city:new(rio, blue),
  AttackedCity = city:aliens(City, [{red, 2}]),
  AlienPool = [{red, 2}],
  FinalCity = city:aliens(AttackedCity, [{red, 3}]),
  ?assertEqual({ok, FinalCity, [{red, 1}]}, alien:attack(Color, AttackedCity, AlienPool)).

attack_an_empty_city_test() ->
  Color = red,
  City = city:new(rio, blue),
  AlienPool = [{red, 2}],
  AttackedCity = city:aliens(City, [{red, 1}]),
  ?assertEqual({ok, AttackedCity, [{red, 1}]}, alien:attack(Color, City, AlienPool)).

attack_with_a_different_alien_type_test() ->
  Color = red,
  City = city:new(rio, blue),
  AttackedCity = city:aliens(City, [{blue, 2}]),
  AlienPool = [{red, 2}],
  FinalCity = city:aliens(AttackedCity, [{blue, 2}, {red, 1}]),
  ?assertEqual({ok, FinalCity, [{red, 1}]}, alien:attack(Color, AttackedCity, AlienPool)).
