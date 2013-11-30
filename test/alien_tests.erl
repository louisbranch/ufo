-module(alien_tests).
-import(alien, [total/1]).
-include_lib("eunit/include/eunit.hrl").

total_empty_test() ->
  0 = total([]).

total_test() ->
  3 = total([{red, 2}, {blue, 1}]).
