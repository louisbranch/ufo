-module(alienation_game).
-export([init/0, start/2, add_player/2]).
-export([players/1, players_deck/1, difficulty/1, current_player/1]).

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
    aliens_deck=alienation_deck:aliens(),
    aliens_discard=[],
    players_deck=alienation_deck:players(),
    players_discard=[],
    hqs_pool=8,
    invasions=0,
    attack_rate=[2,2,3,3,4]
}).

init() -> #state{}.

add_player(State, Name) ->
  Players = players(State),
  NewPlayers = alienation_player:add(Players, Name),
  players(State, NewPlayers).

start(Difficulty, State) ->
  Players = players(State),
  Deck = players_deck(State),
  {DeckDrawn, Hands} = card:initial_hand(Deck, length(Players)),
  PlayersWithHands = alienation_player:distribute_hands(Players, Hands),
  State#state{
    difficulty=Difficulty,
    players=PlayersWithHands,
    players_deck=DeckDrawn,
    current_player=hd(PlayersWithHands)
  }.

%% State getters and setters ----------------------- %%
players(S) -> S#state.players.
players(S, Players) -> S#state{players=Players}.

players_deck(S) -> S#state.players_deck.
difficulty(S) -> S#state.difficulty.

current_player(S) -> S#state.current_player.
