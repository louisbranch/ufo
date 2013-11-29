-module(card_tests).
-import(card, [draw/2, reveal/2, discard/3, initial_hand/2]).
-include_lib("eunit/include/eunit.hrl").

draw_empty_deck_test() ->
  {Deck, Hand} = {[], [{blue, rio}]},
  {game_over, empty_player_deck} = draw(Deck, Hand).

draw_normal_card_deck_test() ->
  {Deck, Hand} = {[{red, ny}, {black, delhi}], [{blue, rio}]},
  {ok, [{black, delhi}], [{red, ny}, {blue, rio}]} = draw(Deck, Hand).

draw_invasion_card_deck_test() ->
  {Deck, Hand} = {[invasion, {black, delhi}], [{blue, rio}]},
  {invasion, [{black, delhi}], [{blue, rio}]} = draw(Deck, Hand).

reveal_empty_deck_test() ->
  {Deck, Pile} = {[], [{blue, rio}]},
  ?assertExit(empty_alien_deck, reveal(Deck, Pile)).

reveal_normal_deck_test() ->
  {Deck, Pile} = {[{red, ny}, {black, delhi}], [{blue, rio}]},
  {ok, {red, ny}, [{black, delhi}], [{red, ny}, {blue, rio}]} = reveal(Deck, Pile).

discard_card_not_in_hand_test() ->
  {Card, Hand, Discard} = {{red, ny}, [{blue, rio}], [{black, delhi}]},
  ?assertExit(card_not_in_hand, discard(Card, Hand, Discard)).

discard_card_in_hand_test() ->
  {Card, Hand, Discard} = {{red, ny}, [{blue, rio}, {red, ny}], [{black, delhi}]},
  {ok, [{blue, rio}], [{red, ny}, {black, delhi}]} = discard(Card, Hand, Discard).

initial_hand_for_two_players_test() ->
  PlayerDeck = [1,2,3,4,5,6,7,8,9,10],
  NewDeck = [9,10],
  Player1 = [1,3,5,7],
  Player2 = [2,4,6,8],
  {ok, NewDeck, [Player1, Player2]} = initial_hand(PlayerDeck, 2).

initial_hand_for_three_players_test() ->
  PlayerDeck = [1,2,3,4,5,6,7,8,9,10],
  NewDeck = [10],
  Player1 = [1,4,7],
  Player2 = [2,5,8],
  Player3 = [3,6,9],
  {ok, NewDeck, [Player1, Player2, Player3]} = initial_hand(PlayerDeck, 3).

initial_hand_for_four_players_test() ->
  PlayerDeck = [1,2,3,4,5,6,7,8,9,10],
  NewDeck = [9,10],
  Player1 = [1,5],
  Player2 = [2,6],
  Player3 = [3,7],
  Player4 = [4,8],
  {ok, NewDeck, [Player1, Player2, Player3, Player4]} = initial_hand(PlayerDeck, 4).

initial_hand_for_five_players_test() ->
  PlayerDeck = [1,2,3,4,5,6,7,8,9,10],
  NewDeck = [],
  Player1 = [1,6],
  Player2 = [2,7],
  Player3 = [3,8],
  Player4 = [4,9],
  Player5 = [5,10],
  {ok, NewDeck, [Player1, Player2, Player3, Player4, Player5]} = initial_hand(PlayerDeck, 5).
