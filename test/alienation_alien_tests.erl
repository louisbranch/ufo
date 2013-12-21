-module(alienation_alien_tests).
-include_lib("eunit/include/eunit.hrl").

attack_with_an_invalid_pool_test() ->
  Type = red,
  City = alienation_city:new(rio, blue),
  AlienPool = [{blue, 2}],
  ?assertExit(alien_type_not_found, alienation_alien:attack(Type, City, AlienPool)).

attack_with_an_invalid_city_aliens_test() ->
  Type = blue,
  City = alienation_city:new(rio, blue),
  InvalidCity = alienation_city:aliens(City, [{blue, 4}]),
  AlienPool = [{blue, 2}],
  ?assertExit(city_aliens_invalid_number, alienation_alien:attack(Type, InvalidCity, AlienPool)).

attack_with_an_empty_pool_test() ->
  Type = red,
  City = alienation_city:new(rio, blue),
  AlienPool = [{red, 0}],
  ?assertEqual({game_over, alien_pool_empty}, alienation_alien:attack(Type, City, AlienPool)).

attack_a_full_city_test() ->
  Type = blue,
  City = alienation_city:new(rio, blue),
  FullCity = alienation_city:aliens(City, [{red, 3}]),
  AlienPool = [{blue, 2}],
  ?assertEqual({invasion, FullCity}, alienation_alien:attack(Type, FullCity, AlienPool)).

attack_an_attacked_city_test() ->
  Type = red,
  City = alienation_city:new(rio, blue),
  AttackedCity = alienation_city:aliens(City, [{red, 2}]),
  AlienPool = [{red, 2}],
  FinalCity = alienation_city:aliens(AttackedCity, [{red, 3}]),
  ?assertEqual({ok, FinalCity, [{red, 1}]}, alienation_alien:attack(Type, AttackedCity, AlienPool)).

attack_an_empty_city_test() ->
  Type = red,
  City = alienation_city:new(rio, blue),
  AlienPool = [{red, 2}],
  AttackedCity = alienation_city:aliens(City, [{red, 1}]),
  ?assertEqual({ok, AttackedCity, [{red, 1}]}, alienation_alien:attack(Type, City, AlienPool)).

attack_with_a_different_alien_type_test() ->
  Type = red,
  City = alienation_city:new(rio, blue),
  AttackedCity = alienation_city:aliens(City, [{blue, 2}]),
  AlienPool = [{red, 2}],
  FinalCity = alienation_city:aliens(AttackedCity, [{blue, 2}, {red, 1}]),
  ?assertEqual({ok, FinalCity, [{red, 1}]}, alienation_alien:attack(Type, AttackedCity, AlienPool)).