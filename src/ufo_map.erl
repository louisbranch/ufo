-module(ufo_map).
-export([cities/0]).
-export([vertices/0]).
-include("ufo_map.hrl").

%% @doc Return list of all cities on the map and their connection names
-spec cities() -> [ufo_city:city()].
cities() ->
    lists:map(fun city/1, ?VERTICES).

%% @doc Create a new city with connections
-spec city({atom(), atom()}) -> ufo_city:city().
city({Name, Type}) ->
    ufo_city:new(Name, Type, connections(Name)).

%% @doc Return list of all connections, connections are cyclic
-spec connections(atom()) -> [atom()].
connections(Name) ->
    Result = lists:foldl(fun(Connection, Acc) ->
                    case Connection of
                        {Origin, Name} -> [Origin|Acc];
                        {Name, Destiny} -> [Destiny|Acc];
                        _ -> Acc
                    end
            end, [], ?EDGES),
    lists:reverse(Result).

-spec vertices() -> [{atom(), atom()}].
vertices() -> ?VERTICES.
