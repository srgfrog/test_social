-module(ws_h).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2,terminate/3]).

-compile([{parse_transform, lager_transform}]).

-define(INFO,lager:info).
-define(ONLYIF(COND,STATE), case (COND) of true -> STATE; false -> nothing end).
-define(ONLYIF(COND,STATE,STATE2), case (COND) of true -> STATE; false -> STATE2 end).
-record(state, {
		username=undefined :: undefined | binary() %% Username of client
	       }).
	        
init(Req, Opts) ->
	{cowboy_websocket, Req, Opts}.

websocket_init(_State) ->
%	erlang:start_timer(1000, self(), <<"Hello!">>),
    erlang:start_timer(1000, self(), <<"ping">>),
    {[], #state{}}.

websocket_handle({text, Msg}, State) ->
    ?INFO("got ~p",[Msg]),
    case catch jsx:decode(Msg) of
	{'EXIT',_} ->
	    ?INFO("Bad JSON ~s",[Msg]),
	    {<<"Bad JSON">>,State};
	JObj -> 
	    case process_json(JObj,State) of
		{[],S} ->
		    {[],S};
		{X,S} when is_binary(X) ->
		    ?INFO("reply ~p",[X]),
		    {[{text,X}],S};
		{L,S} when is_list(L) ->
		    ?INFO("reply ~p",[L]),
		    {[{text,jsx:encode(L)}],S}
	    end
    end;
websocket_handle(_Data, State) ->
    {[], State}.

-spec process_json([tuple()],#state{}) -> {binary() | list(), #state{}}.
process_json([{<<"login">>,Data}],State) ->
    ?INFO("data ~p",[Data]),
    case proplists:get_value(<<"username">>,Data) of
	Username when is_binary(Username) andalso byte_size(Username) > 2 ->
	    add_group(Username),
	    {[{<<"action">>,<<"login">>},{<<"data">>,[{<<"result">>,true}]}],
	     State#state{username=Username}};
	_ ->
	    {[{<<"action">>,<<"login">>},
	      {<<"data">>,[{<<"result">>,false},
			   {<<"error">>,<<"invalid username">>}]}],
	     State#state{username=undefined}}
    end;
process_json([{<<"logout">>,_}],State=#state{username=Username}) ->
    logout(Username),
    {[{<<"action">>,<<"logout">>},{<<"data">>,[{<<"result">>,true}]}],
     State#state{username=undefined}};
process_json([{<<"imessage">>,Msg}],State = #state{username=Username}) when is_binary(Msg)->
    Msg2 = jsx:encode([{<<"action">>,<<"new_im">>},
		     {<<"data">>,[{<<"sender">>,Username},
				   {<<"message">>,Msg}]}]),
    ?INFO("new msg 2 ~p",[Msg2]),
    send_all_client(Msg2),
    {[],State};
process_json(JObj,State) ->
    ?INFO("Unknown command ~p",[JObj]),
    {[],State}.

-spec add_group(binary()) -> ok.
add_group(Username) ->
    case lists:member(Username, pg2:which_groups()) of
	true ->
	    pg2:join(Username,self());
	false ->
	    pg2:create(Username),
	    pg2:join(Username,self())
    end,
    send_list_client().

-spec send_list_client() -> ok.
send_list_client() ->
    L_client = lists:sort([X || X <- pg2:which_groups(), length(pg2:get_members(X)) > 0]),
    Msg = jsx:encode([{<<"action">>,<<"list_client">>},{<<"data">>,L_client}]),
    send_all_client(Msg).

-spec send_all_client(binary()) -> ok.
send_all_client(Msg) ->
    AllPid = lists:flatten([pg2:get_members(X) || X <- pg2:which_groups()]),
    [Pid ! {text,Msg} || Pid <- AllPid],
    ?INFO("sent ~p",[Msg]),
    ok.
			
websocket_info({timeout, _Ref, Msg}, State) ->
    erlang:start_timer(1000, self(), <<"ping">>),
    {[{text, Msg}], State};
websocket_info({text, Msg}, State) ->
    ?INFO("received text ~p",[Msg]),
    {[{text, Msg}], State};
websocket_info(_Info, State) ->
    {[], State}.

logout(undefined) ->
    nothing;
logout(Username) ->
    pg2:leave(Username,self()),
    ?ONLYIF( pg2:get_members(Username) == [], pg2:delete(Username)),
    spawn(fun() -> send_list_client() end).

terminate(_,_,#state{username=Username}) ->
    logout(Username),
    ok;
terminate(_,_,_) ->
    ok.

    
