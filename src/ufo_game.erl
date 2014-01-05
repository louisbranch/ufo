-module(ufo_game).
-export([init/0, start/2, add_player/2]).
-export([players/1, players_deck/1, difficulty/1, current_player/1, map/1,
        diff/2]).

-record(state, {
        difficulty::'easy'|'normal'|'hard',
        players=[]::[ufo_player:player()],
        current_player::ufo_player:player(),
        map=ufo_map:cities()::[ufo_city:city()],
        aliens_pool=ufo_alien:new_pool()::[ufo_alien:pool()],
        aliens_deck=ufo_deck:aliens()::[ufo_card:card()],
        aliens_discard=[]::[ufo_card:card()],
        players_deck=ufo_deck:players()::[ufo_card:card()],
        players_discard=[]::[ufo_card:card()],
        hqs_pool=8::integer(),
        invasions=0::integer(),
        attack_rate=[2,2,3,3,4]::[integer()]
        }).

-opaque state() :: #state{}.

-export_type([state/0]).

%% @doc Create new game state
-spec init() -> state().
init() -> #state{}.

%% @doc Create and add player to players list
-spec add_player(state(), binary()) -> state().
add_player(State, Name) ->
    Players = players(State),
    NewPlayers = ufo_player:add(Players, Name),
    players(State, NewPlayers).

%% @doc
%% Set game difficulty
%% Draw initial hands and distribute to players
%% Set initial players position
%% @end
%% @todo Initial attack
-spec start(state(), atom()) -> state().
start(State, Difficulty) ->
    Players = players(State),
    Deck = players_deck(State),
    {DeckDrawn, Hands} = ufo_card:initial_hand(Deck, length(Players)),
    PlayersWithHands = ufo_player:distribute_hands(Players, Hands),
    InitialMap = ufo_city:initial_position(map(State), PlayersWithHands),
    State#state{
        difficulty=Difficulty,
        players=PlayersWithHands,
        players_deck=DeckDrawn,
        current_player=hd(PlayersWithHands),
        map=InitialMap
        }.

%% @doc Get players
-spec players(state()) -> [ufo_player:player()].
players(S) -> S#state.players.

%% @doc Set players
-spec players(state(), [ufo_player:player()]) -> state().
players(S, Players) -> S#state{players=Players}.

%% @doc Get players deck
-spec players_deck(state()) -> [ufo_card:card()].
players_deck(S) -> S#state.players_deck.

%% @doc Get game difficulty
-spec difficulty(state()) -> atom().
difficulty(S) -> S#state.difficulty.

%% @doc Get current player
-spec current_player(state()) -> ufo_player:player().
current_player(S) -> S#state.current_player.

%% @doc Get game map
-spec map(state()) -> [ufo_city:city()].
map(S) -> S#state.map.

%% @doc Return a diff between two states with relevant info for players
-spec diff(state(), state()) -> [{atom(), term()}].
diff(OldState, NewState) ->
    Comparables = [{players, #state.players},
                   {current_player, #state.current_player},
                   {map, #state.map},
                   {aliens_pool, #state.aliens_pool},
                   {hqs_pool, #state.hqs_pool},
                   {invasions, #state.invasions},
                   {attack_rate, #state.attack_rate}],
    lists:foldl(fun ({Name, Index}, Acc) ->
                Old = erlang:element(Index, OldState),
                New = erlang:element(Index, NewState),
                case New of
                    Old ->
                        Acc;
                    _ when is_list(New) ->
                        List = lists:subtract(New, Old),
                        [{Name, List}|Acc];
                    _ ->
                        [{Name, New}|Acc]
                end
        end, [], Comparables).
