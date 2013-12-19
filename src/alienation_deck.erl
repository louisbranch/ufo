-module(alienation_deck).
-export([aliens/0, players/0]).

aliens() ->
  Cities = alienation_map:vertices(),
  shuffle(Cities).

players() ->
  %FIXME
  Cities = alienation_map:vertices(),
  shuffle(Cities).

shuffle(List) ->
  [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- List])].
