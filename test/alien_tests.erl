-module(alien_tests).
%%-import(alien, [attack/3]).
-include("../src/records.hrl").
-include_lib("eunit/include/eunit.hrl").

attack_a_city_test() ->
  City1 = #city{name=rio, color=blue},
  City2 = #city{name=ny, color=red},
  Map = [City1, City2],
  Aliens = [{blue, 2}, {red, 5}],
  {Map, Aliens}.
