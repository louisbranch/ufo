-module(alienation_game).
-export([init/0, start/2, add_player/2]).
-export([players/1, players_deck/1, difficulty/1]).

-record(state, {
    difficulty,
    players=[],
    current_player,
    cities=map:cities(),
    aliens_pool=[
      {insectoid, 24},
      {grey, 24},
      {reptilian, 24},
      {martian, 24}
    ],
    aliens_deck,
    aliens_discard=[],
    players_deck=deck:players(),
    players_discard=[],
    hqs_pool=8,
    invasions=0,
    attack_rate=0
}).

init() -> #state{}.

add_player(State, Name) ->
  Player = player:new(Name),
  Players = players(State),
  players(State, [Player|Players]).

start(Difficulty, State) ->
  Players = players(State),
  Deck = players_deck(State),
  {DeckDrawn, Hands} = card:initial_hand(Deck, length(Players)),
  PlayersWithHands = player:distribute_hands(Players, Hands),
  State#state{
    difficulty=Difficulty,
    players=PlayersWithHands,
    players_deck=DeckDrawn
  }.

%% State getters and setters ----------------------- %%
players(S) -> S#state.players.
players(S, Players) -> S#state{players=Players}.

players_deck(S) -> S#state.players_deck.
difficulty(S) -> S#state.difficulty.
