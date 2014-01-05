-module(ufo_websockets_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3, websocket_init/3, websocket_handle/3, websocket_info/3,
         websocket_terminate/3]).

-record(state, {game}).

init({tcp, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    case ufo_game_fsm:start_link() of
        {ok, Pid} -> {ok, Req, #state{game=Pid}};
        _ -> {shutdown, Req}
    end.

websocket_handle({text, Msg}, Req, State) ->
    try
        [{Term1, Term2}] = jsx:decode(Msg),
        {Command, Param} = {erlang:binary_to_existing_atom(Term1, utf8), Term2}
    of
        {join, Name} ->
            gen_fsm:send_event(State#state.game, {join, self(), Name}),
            {ok, Req, State}
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
            %%FIXME
            Json = jsx:encode([{ok,true}]),
            {reply, {text, Json}, Req, State};
        _ ->
            {ok, Req, State}
    end.

websocket_terminate(_Reason, _Req, _State) ->
	ok.
