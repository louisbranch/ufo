-module(game_fsm).
-behaviour(gen_fsm).

%% API
-export([start_link/0]).

%% gen_fsm callbacks
-export([init/1,
         registering/2,
         registering/3,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    State = {},
    {ok, registering, State}.

registering({join, _Name}, State) ->
    {next_state, registering, State};

registering(start, State) ->
    {next_state, player_turn, State}.

registering(_Event, _From, State) ->
    {stop, unknown_sync_event, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(current_state, _From, StateName, State) ->
    {reply, {current_state, StateName}, StateName, State}.

handle_info(_Info, _StateName, State) ->
    {stop, unknown_info_event, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%%
%%% user broker -> fsm - game fns
%%% user broker <- fsm
