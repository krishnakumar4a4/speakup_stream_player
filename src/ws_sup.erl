%%%-------------------------------------------------------------------
%% @doc ws top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ws_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
	ChildSpec = #{
		id => speaker_worker, 
		start => {speaker_worker, start_link,[]},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker
		},
    {ok, { {one_for_all, 10, 10}, [ChildSpec]} }.

%%====================================================================
%% Internal functions
%%====================================================================
