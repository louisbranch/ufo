-module(phase).
-export([alien_attack/4]).

alien_attack(Deck, Pool, Discard, Map) ->
  {{Name, Type}, NewDeck, NewDiscard} = alienation_card:reveal(Deck, Discard),
  City = alienation_city:find(Name, Map),
  case alienation_alien:attack(Type, City, Pool) of
    {game_over, Reason} -> {game_over, Reason};
    {invasion, City} -> {invasion, City};
    {ok, _NewCity, NewPool} ->
      %NewMap = alienation_map:update(Map, NewCity),
      {NewDeck, NewPool, NewDiscard, Map}
  end.

