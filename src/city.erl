-module(city).
-export([new/2, find/2, aliens/1, aliens/2]).
-record(city, {name, color, aliens=[], hqs=0}).

new(Name, Color) ->
  #city{name=Name, color=Color}.

aliens(City) -> City#city.aliens.
aliens(City, Aliens) -> City#city{aliens=Aliens}.

find(Name, Map) ->
  case lists:keyfind(Name, #city.name, Map) of
    false -> exit(city_not_found);
    City -> City
  end.

