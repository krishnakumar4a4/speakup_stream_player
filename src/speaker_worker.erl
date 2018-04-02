%%%-------------------------------------------------------------------
%%% @author krishnak <krishnakumar4a4@gmail.com>
%%% @copyright (C) 2018, krishnakumart
%%% @doc
%%%
%%% @end
%%% Created :  2 Apr 2018 by krishnakumart <krishnakumar4a4@gmail.com>
%%%-------------------------------------------------------------------
-module(speaker_worker).

-behaviour(gen_server).

%% API
-export([start_link/0,make_call/1,make_cast/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
	speaker_tcp_client_sock
	}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    {ok,Pid} = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    gen_server:call(?SERVER,{connect},infinity),
    {ok,Pid}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({connect}, _From, State) ->
    SomeHostInNet = "localhost", % to make it runnable on one machine
    case gen_tcp:connect(SomeHostInNet, 9001,[binary, {packet, 0}]) of
		{ok, Sock} ->
			Handshake = ["GET ", "/", " HTTP/1.1\r\n"
                 "Host: ", "localhost:8080", "\r\n"
                 "Connection: Upgrade\r\n"
                 "Sec-WebSocket-Version: 13\r\n"
                 "Sec-WebSocket-Key: ", "JrsCSV40KdotKvRfAzD0dg==", "\r\n"
                 "Upgrade: websocket\r\n",
                 "\r\n"],
                 io:format("sending handshake"),
                 gen_tcp:send(Sock,list_to_binary(Handshake)),
                 {reply,ok,#state{speaker_tcp_client_sock = Sock}};
		Reason ->
			io:format("Unable to start TCP client with Reason ~p~n",[Reason]),
			{reply,error,State}
	end;

handle_call(Request, _From, State) ->
	io:format("~p: handle_call request ~p~n",[?SERVER,Request]),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({receive_from_ws_server,Data}, State) ->
	gen_tcp:send(State#state.speaker_tcp_client_sock,websocket_client:encode_frame(Data)),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

make_call(Req) ->
	gen_server:call(?SERVER,Req).

make_cast(Req) ->
	gen_server:cast(?SERVER,{receive_from_ws_server, Req}).	
