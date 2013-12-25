%% @doc
%% Decks generator based on the
%% cities and special cards available
%% @end
-module(ufo_deck).
-export([aliens/0, players/0]).

%% @doc Create random deck of cards for aliens
-spec aliens() -> [{atom(), atom()}].
aliens() ->
  Cities = ufo_map:vertices(),
  shuffle(Cities).

%% @doc Create random deck of cards for players
%% @todo Add special cards
-spec players() -> [{atom(), atom()}].
players() ->
  Cities = ufo_map:vertices(),
  shuffle(Cities).

%% @doc Shuffles list
-spec shuffle([{atom(), atom()}]) -> [{atom(), atom()}].
shuffle(List) ->
  [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- List])].
