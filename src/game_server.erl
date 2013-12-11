-module(game_server).
-behavior(gen_server).
%% user interface
-export([start_link/1, add_player/2, start_game/2]).
%% gen_server functions
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).
%% state getters intermodules
-export([status/1, difficulty/1, players/1, players_deck/1]).
%% state setters (test only)
-export([players/2]).

-record(state, {
    status=ready,
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

%% Spawn new game
start_link(PlayerName) ->
  gen_server:start_link(?MODULE, [PlayerName], []).

%% Add player to game
add_player(Pid, Name) ->
  gen_server:cast(Pid, {add_player, self(), Name}).

%% Start game
start_game(Pid, Difficulty) ->
  gen_server:cast(Pid, {start, self(), Difficulty}).

init(PlayerName) ->
  Cities = map:cities(),
  PlayersDeck = deck:players(),
  Players = player:new(PlayerName),
  State = #state{
    cities=Cities,
    players_deck=PlayersDeck,
    players=[Players]
  },
  {ok, State}.

%% Handle Sync calls
handle_call(Request, _From, State) ->
  {stop, {unknown_request, Request}, State}.

%% Handle direct messages
handle_info(Message, State) ->
  case Message of
    {state, From} ->
      From ! {ok, State}
  end,
  {noreply, State}.

%% Handle process termination
terminate(_Reason, _State) -> ok.

%% Handle code hot reload
code_change(_PreviousVersion, State, _Extra) ->
  {ok, State}.

%% Handle Async calls ------------------------------ %%

handle_cast({add_player, From, Name}, State) ->
  case status(State) of
    ready ->
      Player = player:new(Name),
      Players = players(State),
      NewState = players(State, [Player|Players]),
      From ! {ok, NewState},
      {noreply, NewState};
    _ ->
      From ! {error, game_status_doesnt_match},
      {noreply, State}
  end;

handle_cast({start, From, Difficulty}, State) ->
  case status(State) of
    ready ->
      Players = players(State),
      PlayersDeck = players_deck(State),
      {PlayersDeckDrawn, Hands} =
        card:initial_hand(PlayersDeck, length(Players)),
      PlayersWithHands = player:distribute_hands(Players, Hands),
      NewState = State#state{
        status=running,
        difficulty=Difficulty,
        players=PlayersWithHands,
        players_deck=PlayersDeckDrawn
      },
      From ! {ok, NewState},
      {noreply, NewState};
    _ ->
      From ! {error, game_status_doesnt_match},
      {noreply, State}
  end.

%% State getters and setters ----------------------- %%
players(S) -> S#state.players.
players(S, Players) -> S#state{players=Players}.

players_deck(S) -> S#state.players_deck.
difficulty(S) -> S#state.difficulty.
status(S) -> S#state.status.
