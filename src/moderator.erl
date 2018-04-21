%%%-------------------------------------------------------------------
%%% @author krishnak <krishnak@inadmin-8.local>
%%% @copyright (C) 2018, krishnak
%%% @doc
%%%
%%% @end
%%% Created :  3 Apr 2018 by krishnak <krishnak@inadmin-8.local>
%%%-------------------------------------------------------------------
-module(moderator).

-behaviour(gen_server).

%% API
-export([start_link/0, cast_to_speaker/2, registered/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    ws_handlers=[],
    participant_count = 0
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
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

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
    ets:new(participant_connections, [set, named_table, {read_concurrency, true}]),
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
handle_call({register, ClientPid, Token, TTT}, From, State) ->
    io:format("~p: registered client ~p, ~p~n",[?MODULE, ClientPid, From]),
    ets:insert(participant_connections, {Token, {TTT, ClientPid, monitor(process,ClientPid)}}),
    {reply, ok, State#state{}};
% handle_call({unregister, ClientPid}, From, State) ->
%     io:format("~p: unregistered client ~p, ~p~n",[?MODULE, ClientPid, From]),
%     RegisteredClients = [I||I<-State#state.ws_handlers, I =/= {ClientPid, From}],
%     {reply, ok, State#state{ws_handlers = RegisteredClients}};
handle_call(registered, _From, State) ->
    Reply = State#state.ws_handlers,
    io:format("registered ~p~n",[Reply]),
    {reply, Reply, State};
handle_call({mute, PartcipantId}, _From, State) ->
    io:format("Muting id ~p~n", [PartcipantId]),
    Participants = State#state.ws_handlers,
    F = fun(A,ClientPid,Id) -> case list_to_binary(integer_to_list(A)) of
                        Id ->
                            erlang:send(ClientPid,{client_on,false}),
                            false;
                        _ ->
                            false
                        end 
        end,
    UpdatedParticipants = [{A,B,C,F(C,A,PartcipantId)}||{A,B,C,_D}<-Participants],
    {reply, ok, State#state{ws_handlers = UpdatedParticipants}};
handle_call({can_speak, PartcipantId}, _From, State) ->
    io:format("can speak id ~p~n", [PartcipantId]),
    Participants = State#state.ws_handlers,
        F = fun(A,ClientPid,Id) -> 
            case list_to_binary(integer_to_list(A)) of
                        Id ->
                            %%Send all the clients false for safety,
                            %%if we are making one of them true
                            erlang:send(ClientPid,{client_on,true}),
                            true;
                        _ ->
                            false
                        end 
        end,
    UpdatedParticipants = [{A,B,C,F(C,A,PartcipantId)}||{A,B,C,_D}<-Participants],
    io:format("UpdatedParticipants ~p~n",[UpdatedParticipants]),
    {reply, ok, State#state{ws_handlers = UpdatedParticipants}}; 

%%Terminates the ws_handler if the client hangs up
handle_call({terminate_ws_handler, Token, TTT}, _From, State) -> 
    io:format("Terminating ws_handler with token ~p and ttt ~p~n",[Token, TTT]),
    case ets:lookup(participant_connections, Token) of
        [] ->
            io:format("No matching TTT to terminate"),
            {reply, ok, State};
        [{Token,{TTT,ClientPid,MonitorRef}}|_] ->
            %%Terminate handler here
            io:format("Terminating ws_handler pid ~p~n",[ClientPid]),
            erlang:send(ClientPid, terminate),
            {reply, ok, State};
        [{Token,{SomeTTT, ClientPid, MonitorRef}}|_] ->
            %% Log this event as suspicon, need not terminate session
            io:format("Terminating ws_handler pid with SomeTTT"),
            {reply, ok, State};
        _ ->
            io:format("Nothing matches"),
            {reply, ok, State}
    end;

handle_call(Request, _From, State) ->
    io:format("Received request ~p~n",[Request]),
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
handle_cast({receive_from_ws_handler, From, Data}, State) ->
    % io:format("~p: Forwarding message to speaker from ~p ~n", [?MODULE, From]),
    speaker_worker:make_cast(Data),
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
handle_info({'DOWN', MonitorRef, _Type, _Object, _Info}, State) ->
    io:format("~p: unregistered client after down ~p ~n",[?MODULE, MonitorRef]),
    case ets:match_object(participant_connections, {'_',{'_','_',MonitorRef}}) of
        [] ->
            {noreply, State};
        [{Token,{TTT,ClientPid,MonitorRef}}|_] ->
            %%Invalidate TTT if the ws_handler is unregistered
            % gen_server:call({global, moderator_worker},{unregister_destroy_ttt, Token, TTT}),
            ets:delete(participant_connections, Token),
            {noreply, State};
        _ ->
            {noreply, State}
    end;
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

cast_to_speaker(Data, From) ->
    gen_server:cast({global,?MODULE},{receive_from_ws_handler, From, Data}).

registered() ->
    gen_server:call({global, ?MODULE}, registered).