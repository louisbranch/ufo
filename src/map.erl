-module(map).
-export([cities/0, connections/0]).

cities() ->
  case file:consult("../map/vertices.erl") of
    {ok, Vertices} ->
      lists:map(fun ({Name, Type}) -> city:new(Name, Type) end, Vertices);
    _ -> exit(vertices_cant_be_loaded)
  end.

connections() ->
  case file:consult("../map/edges.erl") of
    {ok, Edges} -> Edges;
    _ -> exit(edges_cant_be_loaded)
  end.
