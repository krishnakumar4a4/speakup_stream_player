-module(stream).

-behaviour(gen_server).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
    server_socket = undefined
    }).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    gen_server:cast(?MODULE,{connect}).

init([]) ->
    {ok, #state{}}.

handle_call(Request, _From, State) ->
    io:format("Call message ~p~n",[Request]),
    {reply, ignored, State}.

handle_cast({connect}, State) ->
    % SomeHostInNet = "localhost", % to make it runnable on one machine
    % gen_tcp:connect(SomeHostInNet, 9001,[binary, {packet, 0}]),

    {ok, LSock} = gen_tcp:listen(5678, [binary, {packet, 0},{active, false}]),
    {ok, Sock} = gen_tcp:accept(LSock),
    io:format("Connection Accepted on pid ~p~n",[self()]),
    {noreply, State#state{server_socket = Sock}};

handle_cast(Msg, State) ->
    io:format("Cast message ~p~n",[Msg]),
    {noreply, State}.

handle_info({tcp,S,Bin}, State) ->
    io:format("tcp message ~p from ~p ~n",[Bin,S]),
    Socket = State#state.server_socket,
    case Socket of
        undefined ->
            {noreply, State};
        Socket ->
            % gen_tcp:send(State#state.server_socket,Bin),
            {noreply, State}
    end;

handle_info(Info, State) ->
    io:format("Info message ~p~n",[Info]),
    {noreply, State}.

terminate(Reason, _State) ->
    io:format("Terminated with reason ~p~n",[Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
