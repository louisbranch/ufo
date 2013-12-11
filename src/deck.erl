-module(deck).
-export([players/0]).

players() ->
  Cities = map:vertices(),
  shuffle(Cities).

shuffle(List) ->
  [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- List])].
