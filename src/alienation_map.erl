-module(alienation_map).
-export([cities/0]).
-export([vertices/0, edges/0]).

%% @doc
%% Return list of all cities
%% on the map and their connection names
%% @end
-spec cities() -> [alienation_city:city()].
cities() ->
  Vertices = vertices(),
  Edges = edges(),
  lists:map(fun ({Name, Type}) ->
    Connections = connections(Name, Edges),
    alienation_city:new(Name, Type, Connections)
  end, Vertices).

%% @doc
%% Return list of all connections
%% connections are cyclic
%% @end
-spec connections(atom(), [atom()]) -> [atom()].
connections(Name, Connections) ->
  Result = lists:foldl(fun(Connection, Acc) ->
    case Connection of
      {Origin, Name} -> [Origin|Acc];
      {Name, Destiny} -> [Destiny|Acc];
      _ -> Acc
    end
  end, [], Connections),
  lists:reverse(Result).

%% @doc Return list of all map verticies
-spec vertices() -> [tuple(atom(), atom())].
vertices() ->
  case file:consult("../map/vertices.erl") of
    {ok, Vertices} -> Vertices;
    _ -> exit(vertices_cant_be_loaded)
  end.

%% @doc Return list of all map edges
-spec edges() -> [tuple(atom(), atom())].
edges() ->
  case file:consult("../map/edges.erl") of
    {ok, Edges} -> Edges;
    _ -> exit(edges_cant_be_loaded)
  end.
