-module(city).
-export([new/2, find/2, aliens/1, aliens/2,
        connections/3]).
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

connections(City, Connections, Map) ->
  Name = City#city.name,
  Result = lists:foldl(fun(Connection, Acc) ->
    case Connection of
      {Origin, Name} -> [find(Origin, Map)|Acc];
      {Name, Destiny} -> [find(Destiny, Map)|Acc];
      _ -> Acc
    end
  end, [], Connections),
  lists:reverse(Result).
