-module(ufo_game_tests).
-include_lib("eunit/include/eunit.hrl").

add_player_test() ->
    State = ufo_game:init(),
    P1 = ufo_game:add_player(State, luiz),
    ?assertEqual(1, length(ufo_game:players(P1))).

game_start_test_() ->
    InitState = ufo_game:init(),
    P1 = ufo_game:add_player(InitState, luiz),
    P2 = ufo_game:add_player(P1, larissa),
    State = ufo_game:start(P2, normal),
    [test_set_difficulty_level(State),
     test_draw_initial_hands(State),
     test_remove_players_deck_cards(State),
     test_set_current_player(State),
     test_initial_players_position(State)].

test_set_difficulty_level(State) ->
    ?_assertEqual(normal, ufo_game:difficulty(State)).

test_draw_initial_hands(State) ->
    [Player|_P2] = ufo_game:players(State),
    ?_assertEqual(4, length(ufo_player:hand(Player))).

test_remove_players_deck_cards(State) ->
    PlayersDeck = ufo_game:players_deck(State),
    ?_assertEqual(40, length(PlayersDeck)).

test_set_current_player(State) ->
    Player = ufo_game:current_player(State),
    ?_assertEqual(luiz, ufo_player:name(Player)).

test_initial_players_position(State) ->
    Map = ufo_game:map(State),
    Atlanta = ufo_city:find(atlanta, Map),
    CityPlayers = ufo_city:players(Atlanta),
    ?_assertEqual(2, length(CityPlayers)).

diff_state_players_test() ->
    InitState = ufo_game:init(),
    P1 = ufo_game:add_player(InitState, luiz),
    P2 = ufo_game:add_player(P1, larissa),
    Player = ufo_player:new(larissa),
    ?assertEqual([{players, [Player]}], ufo_game:diff(P1, P2)).
