%%%-------------------------------------------------------------------
%% @doc ws public API
%% @end
%%%-------------------------------------------------------------------

-module(ws_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================
start(_Type, _Args) ->
        Dispatch = cowboy_router:compile([
                {'_', [
                        {"/", cowboy_static, {priv_file, websocket, "index.html"}},
                        {"/websocket", ws_handler, []},
                        {"/static/[...]", cowboy_static, {priv_dir, websocket, "static"}}
                ]}
        ]),
        % {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
        %         env => #{dispatch => Dispatch}
        % }),

                PrivDir = code:priv_dir(ws),
        {ok, _} = cowboy:start_tls(https, [
                {port, 8443},
                % {cacertfile, PrivDir ++ "/ssl/cowboy-ca.crt"},
                {certfile, PrivDir ++ "/ssl/localhost.cert"},
                {keyfile, PrivDir ++ "/ssl/localhost.key"}
        ], #{env => #{dispatch => Dispatch}}),
        ws_sup:start_link().

stop(_State) ->
        ok.
