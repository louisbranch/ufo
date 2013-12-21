%% @doc
%% Decks generator based on the
%% cities and special cards available
%% @end
-module(alienation_deck).
-export([aliens/0, players/0]).

%% @doc Create random deck of cards for aliens
-spec aliens() -> [tuple(atom(), atom())].
aliens() ->
  Cities = alienation_map:vertices(),
  shuffle(Cities).

%% @doc Create random deck of cards for players
%% @todo Add special cards
-spec players() -> [tuple()].
players() ->
  Cities = alienation_map:vertices(),
  shuffle(Cities).

%% @doc Shuffles list
-spec shuffle([]) -> [].
shuffle(List) ->
  [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- List])].
