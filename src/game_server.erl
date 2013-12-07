-module(game_server).
-behavior(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

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
start_link(Difficulty) ->
  gen_server:start_link(?MODULE, Difficulty, []).

init(Difficulty) ->
  Cities = map:cities(),
  State = #state{difficulty=Difficulty, cities=Cities},
  {ok, State}.

%% Handle Sync calls
handle_call(Request, _From, State) ->
  {stop, {unknown_request, Request}, State}.

handle_info(Message, State) ->
  case Message of
    {state, From} ->
      From ! {ok, State}
  end,
  {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_PreviousVersion, State, _Extra) ->
  {ok, State}.

%% Handle Async calls
handle_cast(Message, State) ->
  case Message of
    {add_player, From, Player} ->
      From ! {ok, Player},
      {noreply, State}
  end.
