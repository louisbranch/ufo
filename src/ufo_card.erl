-module(ufo_card).
-export([draw/2, reveal/2, discard/3, initial_hand/2]).
-record(city, {
        name::atom(),
        type::atom()
        }).
-record(special, { name::atom() }).
-record(invasion, {}).

-type player_card() :: #city{} | #special{} | #invasion{}.
-type alien_card() :: #city{}.

-opaque card() :: player_card() | alien_card().

-export_type([card/0]).

%% @doc Draw a card from players deck to a player's hand
-spec draw([player_card()], [player_card()]) ->
    {'ok', [player_card()], [player_card()]}
    | {'invasion', [player_card()], [player_card()]}
    | {'game_over', 'empty_player_deck'}.
draw([], _Hand) ->
    {game_over, empty_player_deck};

draw([Card|PlayerDeck], Hand) ->
    case Card of
        #invasion{} -> {invasion, PlayerDeck, Hand};
        _ -> {ok, PlayerDeck, [Card|Hand]}
    end.

%% @doc
%% Reveal a card from the alien deck
%% and discard afterwards to alien discard pile
%% @end
%% @throws 'empty_alien_deck'
-spec reveal([alien_card()], [alien_card()]) ->
    {alien_card(), [alien_card()], [alien_card()]}.
reveal([], _DiscardPile) ->
    throw(empty_alien_deck);

reveal([Card|AlienDeck], DiscardPile) ->
    {Card, AlienDeck, [Card|DiscardPile]}.

%% @doc
%% Discard a card from a player's hand
%% and add to players discard pile
%% @end
-spec discard(player_card(), [player_card()], [player_card()]) ->
    {'ok', [player_card()], [player_card()]}
    | {'error', 'card_not_in_hand'}.
discard(Card, Hand, DiscardPile) ->
    case lists:delete(Card, Hand) of
        Hand -> {error, card_not_in_hand};
        NewHand -> {ok, NewHand, [Card|DiscardPile]}
    end.

%% @doc
%% Give a hand of cards for each player
%% depending on the number of players
%% @end
%% @throws 'invalid_player_deck'
-spec initial_hand([player_card()], integer()) ->
    {[player_card()], [player_card()]}.
initial_hand(PlayerDeck, PlayersNum) ->
    NumCards = case PlayersNum of
        1 -> 5;
        2 -> 4;
        3 -> 3;
        4 -> 2;
        5 -> 2
    end,
    Hands = [[] || _ <- lists:seq(1,PlayersNum)],
    {NewDeck, NewHands} = draw_n_cards(PlayerDeck, Hands, NumCards),
    {NewDeck, NewHands}.

-spec draw_n_cards([player_card()], [player_card()], integer()) ->
    {[player_card()], [player_card()]}.
draw_n_cards(PlayerDeck, Hands, 0) ->
    OrderCards = lists:map(fun lists:reverse/1, Hands),
    {PlayerDeck, OrderCards};

draw_n_cards(PlayerDeck, Hands, NumCards) ->
    {NewDeck, NewHands} = draw_for_each(PlayerDeck, Hands, []),
    draw_n_cards(NewDeck, NewHands, NumCards - 1).

-spec draw_for_each([player_card()], [player_card()], [[player_card()]]) ->
    {[player_card()], [[player_card()]]}.
draw_for_each(PlayerDeck, [], HandsDrew) ->
    {PlayerDeck, lists:reverse(HandsDrew)};

draw_for_each(PlayerDeck, [Hand|Hands], HandsDrew) ->
    case draw(PlayerDeck, Hand) of
        {ok, NewDeck, NewHand} ->
            draw_for_each(NewDeck, Hands, [NewHand|HandsDrew]);
        _ -> throw(invalid_player_deck)
    end.
