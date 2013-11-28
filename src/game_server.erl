-module(game_server).
-export([start/0, loop/1]).
-record(game, {zombie_deck=[], player_deck=[]}).

start() ->
  spawn_link(?MODULE, loop, #game{}).

loop(State) ->
  receive
    {cards@draw, Player, Hand} ->
      NewHand = cards:draw(Hand),
      Player ! NewHand,
      loop(State)
  end.
