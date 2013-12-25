-module(ufo_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3, websocket_init/3, websocket_handle/3, websocket_info/3,
         websocket_terminate/3]).

-record(state, {game}).

init({tcp, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    {ok, Pid} = ufo_game_fsm:start_link(),
	{ok, Req, #state{game=Pid}}.

websocket_handle({text, Msg}, Req, State) ->
    try jsx:decode(Msg) of
        [{<<"join">>, Name}] ->
            gen_fsm:send_event(State#state.game, {join, self(), Name}),
            {ok, Req, State};
        _ -> {reply, {text, << "Unknown comand: ", Msg/binary >>}, Req, State}
    catch
        _:_ -> {reply, {text, << "Unknown comand: ", Msg/binary >>}, Req, State}
    end;

websocket_handle(_Data, Req, State) ->
	{ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
	%erlang:start_timer(1000, self(), <<"How' you doin'?">>),
	{reply, {text, Msg}, Req, State};
websocket_info(Message, Req, State) ->
    case Message of
        {ok, _GameState} ->
            {reply, {text, <<"Ok!">>}, Req, State};
        _ ->
            {ok, Req, State}
    end.

websocket_terminate(_Reason, _Req, _State) ->
	ok.
