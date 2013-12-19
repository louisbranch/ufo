-module(alienation_alien).
-export([attack/3]).

%% Attack city increase its aliens number
%% return types:
%% {ok, NewCity, NewPool}
%% {invasion, City}
%% {game_over, alien_pool_empty}
%% exit(city_aliens_invalid_number)
%% exit(alien_type_not_found)
attack(Type, City, AlienPool) ->
  Aliens = alienation_city:aliens(City),
  case total(Aliens) of
    3 -> {invasion, City};
    N when N >= 0 andalso N < 3 -> attack(Type, City, AlienPool, Aliens);
    _ -> exit(city_aliens_invalid_number)
  end.

attack(Type, City, AlienPool, CityAliens) ->
  {Type, Qty} = find(Type, AlienPool),
  case Qty of
    0 -> {game_over, alien_pool_empty};
    _ ->
      NewPool = remove_from_pool(Type, AlienPool),
      NewAliens = add_to_city(Type, CityAliens),
      NewCity = alienation_city:aliens(City, NewAliens),
      {ok, NewCity, NewPool}
  end.

%% Decrease number of aliens in pool
remove_from_pool(Type, Aliens) ->
  {Type, Qty} = lists:keyfind(Type, 1, Aliens),
  lists:keyreplace(Type, 1, Aliens, {Type, Qty - 1}).

%% Increase number of city aliens
%% or set it to 1
add_to_city(Type, Aliens) ->
  Qty = case lists:keyfind(Type, 1, Aliens) of
    false -> 0;
    {Type, N} -> N
  end,
  lists:keystore(Type, 1, Aliens, {Type, Qty + 1}).

%% Return an alien tuple or
%% exit the process if doesn't exist
find(Type, Aliens) ->
  case lists:keyfind(Type, 1, Aliens) of
    false -> exit(alien_type_not_found);
    Alien -> Alien
  end.

%% Return the total aliens ignoring their types
total(Aliens) ->
  lists:foldl(fun({_, Qty}, Sum) -> Qty + Sum end, 0, Aliens).
