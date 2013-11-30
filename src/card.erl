-module(card).
-export([draw/2, reveal/2, discard/3,
         initial_hand/2]).

%% Draw a card from Players Deck
%% to a player's hand
draw([], _Hand) ->
  {game_over, empty_player_deck};

draw([Card|PlayerDeck], Hand) ->
  case Card of
    invasion -> {invasion, PlayerDeck, Hand};
    _ -> {ok, PlayerDeck, [Card|Hand]}
  end.

%% Reveal a card from the Alien Deck
%% and discard afterwards to Alien
%% Discard Pile
reveal([], _DiscardPile) ->
  exit(empty_alien_deck);

reveal([Card|AlienDeck], DiscardPile) ->
  {Card, AlienDeck, [Card|DiscardPile]}.

%% Discard a card from a player's hand
%% and add to Players Discard Pile
discard(Card, Hand, DiscardPile) ->
  case lists:delete(Card, Hand) of
    Hand -> exit(card_not_in_hand);
    NewHand -> {NewHand, [Card|DiscardPile]}
  end.

%% Give a hand of cards for each player
%% depending the number of players
initial_hand(PlayerDeck, PlayersNum) when PlayersNum > 1, PlayersNum =< 5 ->
  NumCards = case PlayersNum of
    2 -> 4;
    3 -> 3;
    _ -> 2
  end,
  Hands = [[] || _ <- lists:seq(1,PlayersNum)],
  {NewDeck, NewHands} = draw_n_cards(PlayerDeck, Hands, NumCards),
  {NewDeck, NewHands}.

draw_n_cards(PlayerDeck, Hands, 0) ->
  OrderCards = lists:map(fun lists:reverse/1, Hands),
  {PlayerDeck, OrderCards};

draw_n_cards(PlayerDeck, Hands, NumCards) ->
  {NewDeck, NewHands} = draw_for_each(PlayerDeck, Hands, []),
  draw_n_cards(NewDeck, NewHands, NumCards - 1).

draw_for_each(PlayerDeck, [], HandsDrew) ->
  {PlayerDeck, lists:reverse(HandsDrew)};

draw_for_each(PlayerDeck, [Hand|Hands], HandsDrew) ->
  case draw(PlayerDeck, Hand) of
    {ok, NewDeck, NewHand} ->
      draw_for_each(NewDeck, Hands, [NewHand|HandsDrew]);
    _ -> exit(invalid_player_deck)
  end.
