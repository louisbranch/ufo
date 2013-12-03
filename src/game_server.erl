-module(game_server).
-export([start/0, loop/1]).
-record(state, {zombie_deck=[], player_deck=[]}).

start() ->
  spawn_link(?MODULE, loop, [#state{}]).

loop(S) ->
  receive
    {cards@draw, Player, Hand} ->
      Deck = S#state.player_deck,
      case cards:draw(Hand, Deck) of
        {ok, NewDeck, NewHand} -> Player ! NewHand,
        loop(S#state{player_deck=NewDeck})
      end
  end.
