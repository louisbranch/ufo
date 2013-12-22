-module(ufo_action).
-export([new_pool/1, available_options/3]).

%% @doc Return the number of actions for a player on a new turn
-spec new_pool(atom()) -> integer().
new_pool(generalist) -> 5;
new_pool(_Role) -> 4.

%% @doc
%% Return all actions available for a player given their role, hand and
%% surrouding players
%% @end
-spec available_options(atom(), [ufo_card:card()], ufo_city:city()) ->
    [tuple(term(), atom())].
available_options(Role, Hand, City) ->
    CityOptions = city_cards_options(Hand, City),
    DefenseOptions = defense_options(Role, City),
    TradeOptions = trade_options(City),
    lists:flatten([DefenseOptions,TradeOptions,CityOptions]).

-spec city_cards_options([ufo_card:card()], ufo_city:city()) ->
    [{'card', ufo_card:card(), 'fly_from' | 'place_hq' | 'fly_to'}].
city_cards_options([], _City) -> [];
city_cards_options([Card|Hands], City) ->
    case ufo_card:card_from_city(Card, City) of
        true ->
            [{card, Card, fly_from},{card, Card, place_hq}|
             city_cards_options(Hands, City)];
        false ->
            [{card, Card, fly_to}|city_cards_options(Hands, City)]
    end.

-spec defense_options(atom(), ufo_city:city()) -> [{atom(), atom()}].
defense_options(Role, City) ->
    Aliens = ufo_city:aliens(City),
    defense_for_role(Role, Aliens).

-spec defense_for_role(atom(), []) -> [{'defend', atom()}].
defense_for_role(_Role, []) -> [];
defense_for_role(medic, _Aliens) -> [{defend, all}];
defense_for_role(_Role, _Aliens) -> [{defend, one}].

-spec trade_options(ufo_city:city()) -> [{'trade_cards'}].
trade_options(City) ->
    Players = ufo_city:players(City),
    if
        length(Players) > 1 -> [{trade_cards}];
        true -> []
    end.
