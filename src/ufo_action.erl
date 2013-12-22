-module(ufo_action).
-export([new_pool/1, available_options/3]).

%% @doc
%% Return the number of actions
%% for a player on a new turn
%% @end
-spec new_pool(atom()) -> integer().
new_pool(generalist) -> 5;
new_pool(_Role) -> 4.

%% @doc
%% Return all actions available
%% for a player given their role,
%% hand and surrouding players
%% @end
-spec available_options(atom(), [ufo_card:card()], [ufo_city:city()]) ->
    [tuple(atom(), term())].
available_options(_Role, _Hand, _Map) ->
    {ok, ok}.
