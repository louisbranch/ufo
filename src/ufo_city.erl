-module(ufo_city).
-export([new/2, new/3, find/2, aliens/1, aliens/2,
        connections/1]).
-record(city, {
        name::atom(),
        type::atom(),
        connections=[]::list(atom()),
        aliens=[]::list(tuple()),
        hqs=0::integer()
       }).
-opaque city() :: #city{}.
-export_type([city/0]).

%% @doc Create a city with given name and type
-spec new(atom(), atom()) -> city().
new(Name, Type) -> new(Name, Type, []).

%% @doc
%% Create a city with given name, type
%% and other cities connections
%% @end
-spec new(atom(), atom(), [atom()]) -> city().
new(Name, Type, Connections) ->
  #city{name=Name, type=Type, connections=Connections}.

%% @doc Get city aliens
-spec aliens(city()) -> [tuple()].
aliens(City) -> City#city.aliens.

%% @doc Set city aliens
-spec aliens(city(), [tuple()]) -> city().
aliens(City, Aliens) -> City#city{aliens=Aliens}.

%% @doc Get city connections
-spec connections(city()) -> [atom()].
connections(City) -> City#city.connections.

%% @doc Find city on map given its name
-spec find(atom(), [city()]) -> city().
find(Name, Map) ->
  case lists:keyfind(Name, #city.name, Map) of
    false -> exit(city_not_found);
    City -> City
  end.
