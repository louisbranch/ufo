-module(ufo_action_tests).
-include_lib("eunit/include/eunit.hrl").

new_pool_for_generalist_test() ->
    ?assertEqual(5, ufo_action:new_pool(generalist)).

new_pool_for_other_roles_test() ->
    ?assertEqual(4, ufo_action:new_pool(any)).

available_city_cards_options_test() ->
    Role = any,
    Hand = [{city, sp, red}, {city, rio, blue}],
    City = ufo_city:new(rio, blue),
    Options = [
        {card, {city, sp, red}, fly_to},
        {card, {city, rio, blue}, fly_from},
        {card, {city, rio, blue}, place_hq}
    ],
    ?assertEqual(Options, ufo_action:available_options(Role, Hand, City)).

available_defense_options_no_aliens_test() ->
    Role = any,
    Hand = [],
    City = ufo_city:new(rio, blue),
    ?assertEqual([], ufo_action:available_options(Role, Hand, City)).

available_defense_options_medic_role_test() ->
    Role = medic,
    Hand = [],
    City = ufo_city:new(rio, blue),
    AttackedCity = ufo_city:aliens(City, [1,2,3]),
    ?assertEqual([{defend, all}], ufo_action:available_options(Role, Hand, AttackedCity)).

available_defense_options_any_role_test() ->
    Role = any,
    Hand = [],
    City = ufo_city:new(rio, blue),
    AttackedCity = ufo_city:aliens(City, [1,2,3]),
    ?assertEqual([{defend, one}], ufo_action:available_options(Role, Hand, AttackedCity)).

available_trading_options_test() ->
    Role = any,
    Hand = [],
    City = ufo_city:new(rio, blue),
    CityWithPlayers = ufo_city:players(City, [1,2]),
    ?assertEqual([{trade_cards}], ufo_action:available_options(Role, Hand, CityWithPlayers)).
