-module(deck).
-export([aliens/0, players/0]).

aliens() ->
  Cities = map:vertices(),
  shuffle(Cities).

players() ->
  %FIXME
  Cities = map:vertices(),
  shuffle(Cities).

shuffle(List) ->
  [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- List])].
