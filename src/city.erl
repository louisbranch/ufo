-module(city).
-export([new/2, new/3, find/2, aliens/1, aliens/2]).
-record(city, {name, color, connections=[], aliens=[], hqs=0}).

new(Name, Color) -> new(Name, Color, []).

new(Name, Color, Connections) ->
  #city{name=Name, color=Color, connections=Connections}.

aliens(City) -> City#city.aliens.
aliens(City, Aliens) -> City#city{aliens=Aliens}.

find(Name, Map) ->
  case lists:keyfind(Name, #city.name, Map) of
    false -> exit(city_not_found);
    City -> City
  end.
