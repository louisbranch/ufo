-module(ufo_alien).
-export([attack/3]).

%% @doc Attack city increasing its aliens number
%% @throws 'city_aliens_invalid_number' | 'alien_type_not_found'
-spec attack(atom(), ufo_city:city(), [{atom(), integer()}]) ->
      {'ok', ufo_city:city(), [{atom(), integer()}]}
    | {'invasion', ufo_city:city()}
    | {'game_over', 'alien_pool_empty'}.
attack(Type, City, AlienPool) ->
  Aliens = ufo_city:aliens(City),
  case total(Aliens) of
    3 -> {invasion, City};
    N when N >= 0 andalso N < 3 -> attack(Type, City, AlienPool, Aliens);
    _ -> throw(city_aliens_invalid_number)
  end.

attack(Type, City, AlienPool, CityAliens) ->
  {Type, Qty} = find(Type, AlienPool),
  case Qty of
    0 -> {game_over, alien_pool_empty};
    _ ->
      NewPool = remove_from_pool(Type, AlienPool),
      NewAliens = add_to_city(Type, CityAliens),
      NewCity = ufo_city:aliens(City, NewAliens),
      {ok, NewCity, NewPool}
  end.

%% @doc Decrease number of aliens in pool
-spec remove_from_pool(atom(), [{atom(), integer()}]) -> [{atom(), integer()}].
remove_from_pool(Type, Aliens) ->
  {Type, Qty} = lists:keyfind(Type, 1, Aliens),
  lists:keyreplace(Type, 1, Aliens, {Type, Qty - 1}).

%% @doc Increase number of city aliens or set it to 1
-spec add_to_city(atom(), [{atom(), integer()}]) -> [{atom(), integer()}].
add_to_city(Type, Aliens) ->
  Qty = case lists:keyfind(Type, 1, Aliens) of
    false -> 0;
    {Type, N} -> N
  end,
  lists:keystore(Type, 1, Aliens, {Type, Qty + 1}).

%% @doc Return an alien tuple or throw error if doesn't exist
-spec find(atom(), [{atom(), integer()}]) -> {atom(), integer()}.
find(Type, Aliens) ->
  case lists:keyfind(Type, 1, Aliens) of
    false -> throw(alien_type_not_found);
    Alien -> Alien
  end.

%% @doc Return the total aliens ignoring their types
-spec total([{atom(), integer()}]) -> integer().
total(Aliens) ->
  lists:foldl(fun({_, Qty}, Sum) -> Qty + Sum end, 0, Aliens).
