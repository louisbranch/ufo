-module(alien).
-export([attack/3]).

%% Attack city increase its aliens
%% number
%% return types:
%% {ok, NewCity, NewPool}
%% {invasion, City}
%% {game_over, alien_pool_empty}
%% exit(alien_color_not_found)
attack(Color, City, AlienPool) ->
  Aliens = city:aliens(City),
  case total(Aliens) of
    3 -> {invasion, City};
    _ -> attack(Color, City, AlienPool, Aliens)
  end.

attack(Color, City, AlienPool, CityAliens) ->
  {Color, Qty} = find(Color, AlienPool),
  case Qty of
    0 -> {game_over, alien_pool_empty};
    _ ->
      NewPool = remove_from_pool(Color, AlienPool),
      NewAliens = add_to_city(Color, CityAliens),
      NewCity = city:aliens(City, NewAliens),
      {ok, NewCity, NewPool}
  end.

%% Decrease number of aliens in pool
remove_from_pool(Color, Aliens) ->
  {Color, Qty} = lists:keyfind(Color, 1, Aliens),
  lists:keyreplace(Color, 1, Aliens, {Color, Qty - 1}).

%% Increase number of city aliens
%% or set it to 1
add_to_city(Color, Aliens) ->
  Qty = case lists:keyfind(Color, 1, Aliens) of
    false -> 0;
    {Color, N} -> N
  end,
  lists:keystore(Color, 1, Aliens, {Color, Qty + 1}).

%% Return an alien tuple or
%% exit the process if doesn't exist
find(Color, Aliens) ->
  case lists:keyfind(Color, 1, Aliens) of
    false -> exit(alien_color_not_found);
    Alien -> Alien
  end.

%% Return the total aliens
%% ignoring their colors
total(Aliens) ->
  lists:foldl(fun({_, Qty}, Sum) -> Qty + Sum end, 0, Aliens).
