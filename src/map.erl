-module(map).
-export([cities/0]).
-export([vertices/0, edges/0]).

%% Return list of all cities
%% on the map and their connection names
cities() ->
  Vertices = vertices(),
  Edges = edges(),
  lists:map(fun ({Name, Type}) ->
    Connections = connections(Name, Edges),
    city:new(Name, Type, Connections)
  end, Vertices).

%% Return list of all connections
%% between cities as: {Name1, Name2}
%% connections are cyclic
connections(Name, Connections) ->
  Result = lists:foldl(fun(Connection, Acc) ->
    case Connection of
      {Origin, Name} -> [Origin|Acc];
      {Name, Destiny} -> [Destiny|Acc];
      _ -> Acc
    end
  end, [], Connections),
  lists:reverse(Result).

vertices() ->
  case file:consult("../map/vertices.erl") of
    {ok, Vertices} -> Vertices;
    _ -> exit(vertices_cant_be_loaded)
  end.

edges() ->
  case file:consult("../map/edges.erl") of
    {ok, Edges} -> Edges;
    _ -> exit(edges_cant_be_loaded)
  end.
