-module(city).
-export([new/2, find/2]).
-record(city, {name, color, aliens=[], hqs=0}).

new(Name, Color) ->
  #city{name=Name, color=Color}.

find(Name, Map) ->
  case lists:keyfind(Name, #city.name, Map) of
    false -> exit(city_not_found);
    City -> City
  end.

