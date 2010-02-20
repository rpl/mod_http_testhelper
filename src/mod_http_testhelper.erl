%%%----------------------------------------------------------------------
%%% File    : mod_http_testhelper.erl
%%% Author  : Luca Greco <luca.greco [at] alcacoop [dot] it>
%%% Purpose : simple ejabberd http request handler to serve helper useful
%%%           during testing of xmpp code
%%% License : LGPLv3
%%% Created : 2010-02-19
%%% Id      :
%%%----------------------------------------------------------------------

-module(mod_http_testhelper).
-author('luca.greco@alcacoop.it').
-vsn('').
-define(ejabberd_debug, true).

-behaviour(gen_mod).

-export([
    start/2,
    stop/1,
    process/2
    ]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_http.hrl").

%%%----------------------------------------------------------------------
%%% REQUEST HANDLERS
%%%----------------------------------------------------------------------

process([], Request) ->
    {xmlelement, "html", [{"xmlns", "http://www.w3.org/1999/xhtml"}],
     [{xmlelement, "head", [],
       [{xmlelement, "title", [], []}]},
      {xmlelement, "body", [],
       [{xmlelement, "p", [], 
	 [{xmlcdata, io_lib:format("Called query parameters: ~p", [Request#request.q])}]
	}]}]};

process(["json"], _Request=#request{q=[{nokey,[]}]}) ->
    {504, [], {xmlelement, "h1", [],
               [{xmlcdata, "504 Empty Params"}]}};

process(["json"], _Request) ->
    Reply = execute_json_request(_Request#request.q),
    {200, [{"Content-Type", "application/json"}], Reply};

process([_LocalPath], _Request) ->
    {400, [], {xmlelement, "h1", [],
               [{xmlcdata, "400 Bad Request"}]}}.

execute_json_request(Params) ->
    ParamsDict = dict:from_list(Params),
    TasksRaw = dict:fetch("tasks", ParamsDict),
    Reply = case decode_tasks(TasksRaw) of
		{ok, Tasks} -> run_tasks(Tasks);
		{error, Msg} -> {struct,[{success, false},
					 {error_msg, Msg}]}
	    end,
    encode_reply(Reply).

decode_tasks(TasksRaw) ->
    Result = (catch mochijson2:decode(TasksRaw)),
    case Result of
	{'EXIT', _Reason } -> {error, <<"Invalid JSON">>};
	_ -> {ok, Result}
    end.

encode_reply(Reply) ->
    Result = (catch mochijson2:encode(Reply)),
    case Result of
	{'EXIT', _Reason } ->
	    "{\"success\": false, \"error_msg\": \"Problems encoding Reply\"}";
	_ -> Result
    end.

run_tasks(Tasks) ->
    Tasks.

%%% CREATE USER
%%% REMOVE USER
%%% CREATE NODE
%%% DELETE NODE
%%%    mod_pubsub:delete_node(PubSubHost, ["home", Host, User], Owner).

%%%----------------------------------------------------------------------
%%% BEHAVIOUR CALLBACKS
%%%----------------------------------------------------------------------

start(_Host, _Opts) ->
    ok.

stop(_Host) ->
    ok. 
