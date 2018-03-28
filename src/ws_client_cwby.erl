-module(ws_client_cwby).

-behaviour(websocket_client_handler).

-export([
         start_link/0,
         init/2,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3,
         websocket_cast/2
        ]).

start_link() ->
    crypto:start(),
    ssl:start(),
    io:format("Inside other client start"),
    websocket_client:start_link("ws://localhost:9001", ?MODULE, []).

init([], _ConnState) ->
	io:format("client init"),
    {ok, 2}.

websocket_cast(Pid,Data) -> 
    io:format("sending data to node"),
    websocket_client:cast(Pid, Data).

websocket_handle({pong, _}, _ConnState, State) ->
    io:format("received pong"),
	{ok, State};
websocket_handle({text, Msg}, _ConnState, 5) ->
    io:format("Received msg ~p~n", [Msg]),
    {close, <<>>, 10};
websocket_handle({text, Msg}, _ConnState, State) ->
    io:format("Received msg ~p~n", [Msg]),
    timer:sleep(1000),
    BinInt = list_to_binary(integer_to_list(State)),
    {reply, {text, <<"hello, this is message #", BinInt/binary >>}, State + 1}.

websocket_info(start, _ConnState, State) ->
	io:format("started"),
    {reply, {text, <<"erlang message received">>}, State}.

websocket_terminate({close, Code, Payload}, _ConnState, State) ->
    io:format("Websocket closed in state ~p wih code ~p and payload ~p~n",
              [State, Code, Payload]),
    ok;

websocket_terminate(Reason, _ConnState, _State) ->
    io:format("websocket termination with ~p~n",[Reason]).
