-module(alienation_game).
-export([init/0, start/2, add_player/2]).
-export([players/1, players_deck/1, difficulty/1, current_player/1]).

-record(state, {
        difficulty::atom(),
        players=[]::[tuple()],
        current_player::tuple(),
        cities=alienation_map:cities()::[tuple()],
        aliens_pool=[
          {insectoid, 24},
          {grey, 24},
          {reptilian, 24},
          {martian, 24}
            ]::[tuple()],
        aliens_deck=alienation_deck:aliens()::[tuple()],
        aliens_discard=[]::[tuple()],
        players_deck=alienation_deck:players()::[tuple()],
        players_discard=[]::[tuple()],
        hqs_pool=8::integer(),
        invasions=0::integer(),
        attack_rate=[2,2,3,3,4]::[integer()]
}).

%% @doc Create new game state
-spec init() -> #state{}.
init() -> #state{}.

%% @doc Create and add player to players list
-spec add_player(#state{}, binary()) -> #state{}.
add_player(State, Name) ->
  Players = players(State),
  NewPlayers = alienation_player:add(Players, Name),
  players(State, NewPlayers).

%% @doc
%% Set game difficulty
%% Draw initial hands and distribute to players
%% @end
-spec start(#state{}, atom()) -> #state{}.
start(State, Difficulty) ->
  Players = players(State),
  Deck = players_deck(State),
  {DeckDrawn, Hands} = alienation_card:initial_hand(Deck, length(Players)),
  PlayersWithHands = alienation_player:distribute_hands(Players, Hands),
  State#state{
    difficulty=Difficulty,
    players=PlayersWithHands,
    players_deck=DeckDrawn,
    current_player=hd(PlayersWithHands)
  }.

%% @doc Get players
-spec players(#state{}) -> [tuple()].
players(S) -> S#state.players.

%% @doc Set players
-spec players(#state{}, [tuple()]) -> #state{}.
players(S, Players) -> S#state{players=Players}.

%% @doc Get players deck
-spec players_deck(#state{}) -> [tuple()].
players_deck(S) -> S#state.players_deck.

%% @doc Get game difficulty
-spec difficulty(#state{}) -> atom().
difficulty(S) -> S#state.difficulty.

%% @doc Get current player
-spec current_player(#state{}) -> tuple().
current_player(S) -> S#state.current_player.
