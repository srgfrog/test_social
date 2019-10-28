%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(server_app).
-behaviour(application).

%% API.
-export([start/2,start/0]).
-export([stop/1]).

%% API.

start() ->
    application:ensure_all_started(server).
%    start([],[]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
				      {'_', [
					     {"/", cowboy_static, {file, "elm18/index.html"}},
					     {"/websocket", ws_h, []}
					    ]}
				     ]),
    
    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
							 env => #{dispatch => Dispatch}
							}),
    spawn(fun() -> ws_h:maybe_create_tables(mnesia:system_info(tables)) end),
    websocket_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(http).

