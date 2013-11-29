-module(game_server).
-export([start/0, loop/1]).
-include("records.hrl").

start() ->
  spawn_link(?MODULE, loop, [#game{}]).

loop(State) ->
  receive
    {cards@draw, Player, Hand} ->
      Deck = State#game.player_deck,
      case cards:draw(Hand, Deck) of
        {ok, NewDeck, NewHand} -> Player ! NewHand,
        loop(State#game{player_deck=NewDeck})
      end
  end.
