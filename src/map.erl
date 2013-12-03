-module(map).
-behavior(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

start_link() ->
  gen_server:start_link(?MODULE, [], []).

init([]) ->
  Graph = digraph:new(),
  add_vertices(Graph),
  add_edges(Graph),
  {ok, Graph}.

handle_call(Request, _From, Graph) ->
  case Request of
    {path, Origin, Destiny} ->
      Path = path(Graph, Origin, Destiny),
      {reply, Path, Graph};
    _ -> {noreply, Graph}
  end.

handle_cast(_Message, Graph) ->
  {noreply, Graph}.

handle_info(_Message, Graph) ->
  {noreply, Graph}.

terminate(_Reason, Graph) ->
  {stop, normal, Graph}.

code_change(_PreviousVersion, Graph, _Extra) ->
  {ok, Graph}.

path(Graph, Origin, Destiny) ->
  V1 = vertice(Graph, Origin),
  V2 = vertice(Graph, Destiny),
  case digraph:get_path(Graph, V1, V2) of
    false -> {error, no_path};
    Vertices -> Vertices
  end.

vertice(Graph, Name) ->
  digraph:vertex(Graph, Name).

add_vertices(Graph) ->
  case file:consult("../map/vertices.erl") of
    {ok, Vertices} ->
      lists:map(fun ({Name, _Type}) -> digraph:add_vertex(Graph, Name) end, Vertices);
    _ -> exit(vertices_cant_be_loaded)
  end.

add_edges(Graph) ->
  case file:consult("../map/edges.erl") of
    {ok, Edges} ->
      lists:map(fun ({E1, E2}) ->
        V1 = digraph:vertex(Graph, E1),
        V2 = digraph:vertex(Graph, E2),
        digraph:add_edge(Graph, V1, V2),
        digraph:add_edge(Graph, V2, V1)
      end, Edges);
    _ -> exit(edges_cant_be_loaded)
  end.
