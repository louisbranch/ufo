-module(alien).
-export([attack/3]).
-include("records.hrl").

attack(CityName, Map, Aliens) ->
  City = findCity(CityName, Map),
  case addAlien(Aliens, City#city.color) of
    {game_over, Reason} -> {game_over, Reason};
    {ok, NewAliens} ->
      NewAliens
  end.

addAlien(Aliens, Color) ->
  Alien = findAlien(Color, Aliens),
  if
    Alien =:= 0 -> {game_over, alien_color_reached_zero};
    Alien > 0 ->
      NewAliens = lists:keyreplace(Color, 1, Aliens, {Color, Alien - 1}),
      {ok, NewAliens};
    true -> exit(alien_color_number_mismatch)
  end.

findCity(Name, Map) ->
  case lists:keyfind(Name, #city.name, Map) of
    false -> exit(city_not_found);
    City -> City
  end.

findAlien(Color, Aliens) ->
  case lists:keyfind(Color, 1, Aliens) of
    false -> exit(alien_color_not_found);
    Alien -> Alien
  end.
