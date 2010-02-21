%%%----------------------------------------------------------------------
%%% File    : mod_http_testhelper.erl
%%% Author  : Luca Greco <luca.greco@alcacoop.it>
%%% Purpose : simple ejabberd http request handler to serve helper useful
%%%           during testing of xmpp code
%%% License : GPLv2
%%% Created : 19 Feb 2010 by Luca Greco <luca.greco@alcacoop.it>
%%% Id      :
%%%----------------------------------------------------------------------
%%%
%%% mod_http_testhelper.erl, Copyright (C) 2010 Alca Società Cooperativa
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_http_testhelper).
-author('luca.greco@alcacoop.it').
-vsn('').
-define(ejabberd_debug, true).

-behaviour(gen_mod).
-compile(export_all).
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
	Result when is_list(Result) -> {ok, Result};
	_ -> {error, <<"tasks must be an array">>}
    end.

encode_reply(Reply) ->
    Result = (catch mochijson2:encode(Reply)),
    case Result of
	{'EXIT', _Reason} ->
	    "{\"success\": false, \"error_msg\": \"Problems encoding Reply\"}";
	_ -> Result
    end.

run_tasks([Task|Rest]) ->
    [run_tasks(Task)|run_tasks(Rest)];
run_tasks([]) ->
    [];
run_tasks({struct, Attrs}=Task) ->
    run_task(Attrs);
run_tasks(Task) ->
    {struct, [{success, false}, {error_msg, <<"Task have to be a json object">>}, {task, Task}]}.
 
run_task(TaskAttrs) ->
    Attrs = dict:from_list(TaskAttrs),
    TaskName = dict:fetch(<<"name">>, Attrs),
    TaskId = dict:fetch(<<"id">>, Attrs),
    TaskResult = (catch run_task_by_name(TaskName, Attrs)),
    case TaskResult of
	{'EXIT', Reason} -> ReasonString = lists:flatten(io_lib:format("~p", [Reason])),
			    {struct, [{success, false},  
				      {task_id, TaskId},
				      {error, <<"exception_executing_task">>},
				      {error_msg, list_to_binary(ReasonString)}]};
	{ok, Result} -> {struct, [{success, true},
				  {task_id, TaskId},
				  {task_result, TaskResult}]};
	{error, Type, Msg} -> {struct, [{success, false},
				  {task_id, TaskId},
				  {error, Type},
				  {error_msg, Msg}]}
    end.

run_task_by_name(<<"user_create">>, Attrs) ->
    User = dict:fetch(<<"user">>, Attrs),
    Host = dict:fetch(<<"host">>, Attrs),
    Password = dict:fetch(<<"password">>, Attrs),
    create_user(User,Host,Password);
run_task_by_name(<<"user_delete">>, Attrs) ->
    User = dict:fetch(<<"user">>, Attrs),
    Host = dict:fetch(<<"host">>, Attrs),
    delete_user(User,Host);
run_task_by_name(<<"pubsub_create_node">>, Attrs) ->
    OwnerJid = dict:fetch(<<"owner_jid">>, Attrs),
    PubSubHost = dict:fetch(<<"pubsub_host">>, Attrs),
    PubSubNode = dict:fetch(<<"node">>, Attrs),
    pubsub_create_node(PubSubHost,PubSubNode,OwnerJid);
run_task_by_name(<<"pubsub_delete_node">>, Attrs) ->
    OwnerJid = dict:fetch(<<"owner_jid">>, Attrs),
    PubSubHost = dict:fetch(<<"pubsub_host">>, Attrs),
    PubSubNode = dict:fetch(<<"node">>, Attrs),
    pubsub_delete_node(PubSubHost,PubSubNode,OwnerJid);
run_task_by_name(<<"pubsub_create_home_node">>, Attrs) ->
    OwnerJid = dict:fetch(<<"owner_jid">>, Attrs),
    PubSubHost = dict:fetch(<<"pubsub_host">>, Attrs),
    PubSubNode = pubsub_home_node_from_jid(OwnerJid),
    pubsub_create_node(PubSubHost,PubSubNode,OwnerJid);
run_task_by_name(<<"pubsub_delete_home_node">>, Attrs) ->
    OwnerJid = dict:fetch(<<"owner_jid">>, Attrs),
    PubSubHost = dict:fetch(<<"pubsub_host">>, Attrs),
    PubSubNode = pubsub_home_node_from_jid(OwnerJid),
    pubsub_delete_node(PubSubHost,PubSubNode,OwnerJid);
run_task_by_name(<<"pubsub_delete_all_node">>, Attrs) ->
    Result1 = run_task_by_name(<<"pubsub_delete_home_node">>,Attrs),
    Result2 = run_task_by_name(<<"pubsub_create_home_node">>,Attrs),
    case [Result1,Result2] of
	[{ok,_},{ok,_}] -> {ok,<<"pubsub_all_node_deleted">>};
	_ -> {error,<<"error_deleting_all_nodes">>, <<"">>}
    end;
run_task_by_name(_Name,_Attrs) ->
    {error, <<"unknown_task_name">>, <<"">>}.

pubsub_home_node_from_jid(JidString) ->
    Jid = jlib:string_to_jid(JidString),
    ["home", Jid#jid.server, Jid#jid.user].
    
create_user(User, Server, Password) ->
    Result = ejabberd_auth:try_register(User, Server, Password),
    case result of
	{atomic, ok} -> {ok, <<"user_created">>};
	{atomic, exists} -> {ok, <<"user_exists">>};
	{error, not_allowed} -> {error, <<"registering_not_allowed">>, <<"">>};
        _ -> {error, <<"unkown_result">>, <<"">>}
    end.

delete_user(User, Server) ->
    {ok, <<"User Deleted">>}.

pubsub_create_node(PubSubHost, PubSubNode, OwnerJid) ->
    {ok, <<"PubSub Node Created">>}.

pubsub_delete_node(PubSubHost, PubSubNode, OwnerJid) ->
    {ok, <<"PubSub Node Deleted">>}.

%%% TODO:
%%% CREATE USER
%%%    ejabberd_auth:try_register(User, Server, Password) -> {atomic, ok|exists} | {error, not_allowed}
%%% REMOVE USER
%%%    ejabberd_auth:get_password(User, Server) -> Password | false
%%%    ejabberd_auth:remove_user(User,Server,Password) -> ok | not_exists | not_allowed | bad_request | error
%%% CREATE NODE
%%%    mod_pubsub? o nodetree_default?
%%%    nodetree_default:set_node(Record)
%% #pubsub_node{nodeid = {"pubsub.acetone.inland",
%%                              ["home","acetone.inland"]},
%%                    parentid = {"pubsub.acetone.inland",["home"]},
%%                    type = "default",
%%                    owners = [{[],"pubsub.acetone.inland",[]}],
%%                    options = [{node_type,default},
%%                               {deliver_payloads,true},
%%                               {notify_config,false},
%%                               {notify_delete,false},
%%                               {notify_retract,true},
%%                               {persist_items,true},
%%                               {max_items,10},
%%                               {subscribe,true},
%%                               {access_model,open},
%%                               {roster_groups_allowed,[]},
%%                               {publish_model,publishers},
%%                               {max_payload_size,60000},
%%                               {send_last_published_item,on_sub_and_presence},
%%                               {deliver_notifications,true},
%%                               {presence_based_delivery,false}]}
%%% DELETE NODE
%%%    mod_pubsub:delete_node(PubSubHost, ["home", Host, User], Owner).

%%%----------------------------------------------------------------------
%%% BEHAVIOUR CALLBACKS
%%%----------------------------------------------------------------------

start(_Host, _Opts) ->
    ok.

stop(_Host) ->
    ok. 
