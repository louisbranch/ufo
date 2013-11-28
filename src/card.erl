-module(card).
-export([draw/2, draw/3]).

draw(Deck, Hand) ->
  draw(Deck, Hand, 1).

draw(Deck, Hand, 0) ->
  {drew, Deck, Hand};

draw(Deck, Hand, N) ->
  [Card, NewDeck] = Deck,
  draw(NewDeck, [Card, Hand], N - 1).

