-module(ws_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([onOpen/0,onClose/0,onMessage/1]).

init(Req, Opts) ->
	io:format("Init with ~p~n",[Req]),
	{cowboy_websocket, Req, Opts}.

websocket_init(State) ->
	io:format("sending hello back ~p~n",[State]),
	%%Connect to TCP server
	% stream:start_link(),
	SomeHostInNet = "localhost", % to make it runnable on one machine
    case gen_tcp:connect(SomeHostInNet, 9001,[binary, {packet, 0}]) of
		{ok, Sock} ->
			Handshake = ["GET ", "/", " HTTP/1.1\r\n"
                 "Host: ", "localhost:8080", "\r\n"
                 "Connection: Upgrade\r\n"
                 "Sec-WebSocket-Version: 13\r\n"
                 "Sec-WebSocket-Key: ", "JrsCSV40KdotKvRfAzD0dg==", "\r\n"
                 "Upgrade: websocket\r\n",
                 % [ [Header, ": ", Value, "\r\n"] || {Header, Value} <- ExtraHeaders],
                 "\r\n"],
                 gen_tcp:send(Sock,list_to_binary(Handshake)),
                 {ok,[{tcp_client_sock, Sock}|State]};
		Reason ->
			io:format("Unable to start TCP client with Reason ~p~n",[Reason]),
			{ok,[]}
	end.

	%%erlang:start_timer(1000, self(), <<"Hello!">>),
	%% UDP client
	% {ok, Socket} = gen_udp:open(0, [binary]),
	% {ok,[{udp_socket,Socket}|State]}.

	%%Websocket client
	% case ws_client_cwby:start_link() of
	% 	{ok, Pid} ->
	% 		{ok,[{ws_client_cwby,Pid}|State]};
	% 	Reason ->
	% 		io:format("Unable to start other client with Reason ~p~n",[Reason]),
	% 	{ok,[]}
	% end.

websocket_handle({text, Msg}, State) ->
	io:format("received text ~p and state is ~p ~n", [Msg, State]),
	{reply, {text, << "That's what she said! ", Msg/binary >>}, State};
websocket_handle(Data, State) ->
	DataSize = binary:referenced_byte_size(element(2,Data)),
	io:format("Received data ~p~n",[Data]),
	%%UDP sending
	% ok = gen_udp:send(element(2,hd(State)), "localhost", 10000, element(2,Data)),
	%%Websocket casting
	% if
	%  	(DataSize>256) and (length(State)>1) ->
	% 			ws_client_cwby:websocket_cast(element(2,hd(State)),{binary,<<147,4,192,0>>});
	% 			true ->
	% 				ok
	% end,
	% ws_client_cwby:websocket_cast(element(2,hd(State)),Data),
	% if
	%  	DataSize > 256 ->
	% 			ws_client_cwby:websocket_cast(element(2,hd(State)),{binary,<<147,3,192,0>>});			
	% 			true ->
	% 				ok
	% end,
	NewState = case DataSize of
				256 ->
					State++[{handshake,true}];
				_ ->
					State
	end,
	<< MaskingKey:32 >> = <<221,183,73,218>>,
	% gen_tcp:send(element(2,hd(State)),erlang:list_to_binary([<<130,132,221,183,73,218>>,websocket_client:mask_payload(MaskingKey,element(2,Data))])),
	gen_tcp:send(element(2,hd(State)),websocket_client:encode_frame(Data)),
	{ok, NewState}.

websocket_info({timeout, _Ref, Msg}, State) ->
	io:format("Timeout!!! ~p~n",[Msg]),
	erlang:start_timer(1000, self(), <<"How' you doin'?">>),
	{reply, {text, Msg}, State};
websocket_info(Info, State) ->
	io:format("Some thing random ~p~n",[Info]),
	{ok, State}.

onOpen() -> 
	io:format("Closed from other side").

onClose() -> 
	io:format("open from other side").

onMessage(D) ->
	io:format("Message received from other side ~p~n",[D]).
