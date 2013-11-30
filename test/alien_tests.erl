-module(alien_tests).
-include_lib("eunit/include/eunit.hrl").

total_empty_test() ->
  0 = alien:total([]).

total_test() ->
  3 = alien:total([{red, 2}, {blue, 1}]).
