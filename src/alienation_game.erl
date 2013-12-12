-module(alienation_game).
-export([init/1, start/2, add_player/2]).
-export([players/1, players_deck/1, difficulty/1]).

-record(state, {
    difficulty,
    players=[],
    current_player,
    cities,
    aliens_pool=[
      {insectoid, 24},
      {grey, 24},
      {reptilian, 24},
      {martian, 24}
    ],
    aliens_deck,
    aliens_discard=[],
    players_deck,
    players_discard=[],
    hqs_pool=8,
    invasions=0,
    attack_rate=0
}).

init(PlayerName) ->
  Cities = map:cities(),
  PlayersDeck = deck:players(),
  Players = player:new(PlayerName),
  #state{
    cities=Cities,
    players_deck=PlayersDeck,
    players=[Players]
  }.

add_player(State, Name) ->
  Player = player:new(Name),
  Players = players(State),
  players(State, [Player|Players]).

start(Difficulty, State) ->
  Players = players(State),
  PlayersDeck = players_deck(State),
  {PlayersDeckDrawn, Hands} = card:initial_hand(PlayersDeck, length(Players)),
  PlayersWithHands = player:distribute_hands(Players, Hands),
  State#state{
    difficulty=Difficulty,
    players=PlayersWithHands,
    players_deck=PlayersDeckDrawn
  }.

%% State getters and setters ----------------------- %%
players(S) -> S#state.players.
players(S, Players) -> S#state{players=Players}.

players_deck(S) -> S#state.players_deck.
difficulty(S) -> S#state.difficulty.
