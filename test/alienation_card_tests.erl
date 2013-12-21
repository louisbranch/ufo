-module(alienation_card_tests).
-include_lib("eunit/include/eunit.hrl").

draw_empty_deck_test() ->
    {Deck, Hand} = {[], [{blue, rio}]},
    ?assertEqual({game_over, empty_player_deck}, alienation_card:draw(Deck, Hand)).

draw_normal_card_deck_test() ->
    {Deck, Hand} = {[{red, ny}, {black, delhi}], [{blue, rio}]},
    ?assertEqual({ok, [{black, delhi}], [{red, ny}, {blue, rio}]}, alienation_card:draw(Deck, Hand)).

draw_invasion_card_deck_test() ->
    {Deck, Hand} = {[{invasion}, {black, delhi}], [{blue, rio}]},
    ?assertEqual({invasion, [{black, delhi}], [{blue, rio}]}, alienation_card:draw(Deck, Hand)).

reveal_empty_deck_test() ->
    {Deck, Pile} = {[], [{blue, rio}]},
    ?assertExit(empty_alien_deck, alienation_card:reveal(Deck, Pile)).

reveal_normal_deck_test() ->
    {Deck, Pile} = {[{red, ny}, {black, delhi}], [{blue, rio}]},
    ?assertEqual({{red, ny}, [{black, delhi}], [{red, ny}, {blue, rio}]}, alienation_card:reveal(Deck, Pile)).

discard_card_not_in_hand_test() ->
    {Card, Hand, Discard} = {{red, ny}, [{blue, rio}], [{black, delhi}]},
    ?assertExit(card_not_in_hand, alienation_card:discard(Card, Hand, Discard)).

discard_card_in_hand_test() ->
    {Card, Hand, Discard} = {{red, ny}, [{blue, rio}, {red, ny}], [{black, delhi}]},
    ?assertEqual({[{blue, rio}], [{red, ny}, {black, delhi}]}, alienation_card:discard(Card, Hand, Discard)).

initial_hand_for_two_players_test() ->
    PlayerDeck = [1,2,3,4,5,6,7,8,9,10],
    NewDeck = [9,10],
    P1 = [1,3,5,7],
    P2 = [2,4,6,8],
    ?assertEqual({NewDeck, [P1, P2]}, alienation_card:initial_hand(PlayerDeck, 2)).

initial_hand_for_three_players_test() ->
    PlayerDeck = [1,2,3,4,5,6,7,8,9,10],
    NewDeck = [10],
    P1 = [1,4,7],
    P2 = [2,5,8],
    P3 = [3,6,9],
    ?assertEqual({NewDeck, [P1, P2, P3]}, alienation_card:initial_hand(PlayerDeck, 3)).

initial_hand_for_four_players_test() ->
    PlayerDeck = [1,2,3,4,5,6,7,8,9,10],
    NewDeck = [9,10],
    P1 = [1,5],
    P2 = [2,6],
    P3 = [3,7],
    P4 = [4,8],
    ?assertEqual({NewDeck, [P1, P2, P3, P4]}, alienation_card:initial_hand(PlayerDeck, 4)).

initial_hand_for_five_players_test() ->
    PlayerDeck = [1,2,3,4,5,6,7,8,9,10],
    NewDeck = [],
    P1 = [1,6],
    P2 = [2,7],
    P3 = [3,8],
    P4 = [4,9],
    P5 = [5,10],
    ?assertEqual({NewDeck, [P1, P2, P3, P4, P5]}, alienation_card:initial_hand(PlayerDeck, 5)).

initial_hand_with_invalid_deck_test() ->
    PlayerDeck = [],
    ?assertExit(invalid_player_deck, alienation_card:initial_hand(PlayerDeck, 2)).

