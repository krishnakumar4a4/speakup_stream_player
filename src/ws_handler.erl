-module(ws_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([websocket_terminate/3]).
-export([onOpen/0,onClose/0,onMessage/1]).
-record(state,{
	client_on = true,
	token,
	ttt
	}).

init(Req, Opts) ->
	% io:format("Init with ~p~n",[Req]),
	case binary:split(cowboy_req:qs(Req),<<"=">>) of
		[<<"ttt">>,TTT] ->
			io:format("~p: Query value is ~p ~n",[?MODULE, TTT]),
			Cookie = proplists:get_value(<<"participant_cookie">>,cowboy_req:parse_cookies(Req)),
			io:format("~p: cookies from req are ~p~n",[?MODULE,Cookie]),
			case gen_server:call({global,moderator_worker},{validate_ttt, Cookie, TTT}) of
				ok ->
					{cowboy_websocket, Req, [{token, Cookie},{ttt, TTT}], maps:new()};
				donotexist ->
					{error, "Cannot start"}
				end;
			% validate_with_moderator()
		_Val ->
			io:format("~p: Invalid attempt ~p~n",[?MODULE, _Val])
	end.

% apply_filler(State,Pid) ->
% 	io:format("Started applying filler~n"),
% 	timer:send_interval(1,Pid,{binary,list_to_binary([<<147,2,218,16,0>>,<<0>>,<<255:(4099*8)>>])}).

websocket_handle({text, Msg}, State) ->
	io:format("received text ~p and state is ~p ~n", [Msg, State]),
	{reply, {text, << "That's what she said! ", Msg/binary >>}, State};
	
websocket_handle(Data, State) ->
	case State#state.client_on of
		true ->
			% io:format("~p: Got data in handle and sending it to moderator from ~p ~n",[?MODULE, self()]),
			moderator:cast_to_speaker(Data, self());
		false ->
			ok
	end,
	{ok,State}.

websocket_init(State) ->
	io:format("sending hello back ~p~n",[State]),
	[{token, Token}, {ttt, TTT}] = State,
	gen_server:call({global,moderator},{register,self(), Token, TTT}),
	{ok,#state{ token = Token, ttt = TTT}}.

websocket_info({timeout, _Ref, Msg}, State) ->
	io:format("Timeout!!! ~p~n",[Msg]),
	{reply, {text, Msg}, State};
websocket_info({binary,Data}, State) ->
	case State#state.client_on of
		true ->
			% io:format("~p: Got data in info and sending it to moderator from ~p ~n",[?MODULE, self()]),
			moderator:cast_to_speaker({binary,Data}, self());
		false ->
			ok
	end,
	{ok, State};
websocket_info({client_on,true}, State) ->
	io:format("Turning on client to speak ~p~n",[self()]),
	{ok, State#state{client_on = true}};
websocket_info({client_on,false}, State) ->
	io:format("Turning off client to speak ~p~n",[self()]),
	{ok, State#state{client_on = false}};
websocket_info(terminate, State) ->
	io:format("Client hung up, terminating ws_handler ~p~n",[self()]),
	{stop, State};
websocket_info(Info, State) ->
	io:format("Some thing random ~p~n",[Info]),
	{ok, State}.

websocket_terminate(Reason, _Req, _State) ->
	io:format("ws_handler terinating with ~p~n",[Reason]),
	gen_server:call({global,moderator},{unregister,self()}),
	ok.

onOpen() -> 
	io:format("Closed from other side").

onClose() -> 
	io:format("open from other side").

onMessage(D) ->
	io:format("Message received from other side ~p~n",[D]).
