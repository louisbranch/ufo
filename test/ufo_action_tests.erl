-module(ufo_action_tests).
-include_lib("eunit/include/eunit.hrl").

new_pool_for_generalist_test() ->
    ?assertEqual(5, ufo_action:new_pool(generalist)).

new_pool_for_other_roles_test() ->
    ?assertEqual(4, ufo_action:new_pool(any)).

