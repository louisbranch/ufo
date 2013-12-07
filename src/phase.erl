-module(phase).
-export([alien_attack/4]).

alien_attack(Deck, Pool, Discard, Map) ->
  {{Name, Type}, NewDeck, NewDiscard} = card:reveal(Deck, Discard),
  City = city:find(Name, Map),
  case alien:attack(Type, City, Pool) of
    {game_over, Reason} -> {game_over, Reason};
    {invasion, City} -> {invasion, City};
    {ok, NewCity, NewPool} ->
      NewMap = map:update(Map, NewCity),
      {NewDeck, NewPool, NewDiscard, NewMap}
  end.

