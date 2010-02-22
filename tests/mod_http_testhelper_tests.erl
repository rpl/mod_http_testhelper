-module(mod_http_testhelper_tests).

-include("eunit/include/eunit.hrl").

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_http.hrl").

rootpage_test() ->
    Result = mod_http_testhelper:process([], #request{q={nokey,[]}}),
    ?_assertMatch({xmlelement, "html", _Attrs , _Childs}, Result).

jsonpage_without_queryparams_test() ->
    Result = mod_http_testhelper:process(["json"], #request{q=[{nokey,[]}]}),
    ?_assertMatch({504, _Headers, _Body}, Result).
    
nonexistent_paths_test() ->
    Result = mod_http_testhelper:process(["nonexistent"], #request{}),
    ?_assertMatch({400, _Headers, _Body}, Result).

%%% DESC: Test for json response body on missing tasks query param
jsonpage_executing_missingtasks_queryparam_test() ->
    Result = mod_http_testhelper:process(["json"], #request{q=[{invalid_param, "value"}]}),
    %%% test Result match a HTTP Status 200 reply
    ?assertMatch({200, [{"Content-Type", "application/json"}], _Body}, Result),
    %%% extract and json decode result body
    DecodedBody = decode_json_body(Result),
    %%% test error json object content
    ?assertMatch({struct, [{<<"success">>, false},
			  {<<"error">>, <<"missing_tasks">>}, _ErrorMsg]}, DecodedBody).

%%% DESC: Test for json response body on invalid json in task query param
jsonpage_executing_invalidjson_tasks_queryparam_test() ->
    Result = mod_http_testhelper:process(["json"], #request{q=[{"tasks", "{\"invalid_json\": 1"}]}),
    %%% test Result match a HTTP Status 200 reply
    ?assertMatch({200, [{"Content-Type", "application/json"}], _Body}, Result),
    %%% extract and json decode result body
    DecodedBody = decode_json_body(Result),
    %%% test error json object content
    ?assertMatch({struct, [{<<"success">>, false},
			  {<<"error">>, <<"invalid_json">>}, _ErrorMsg]}, DecodedBody).



jsonpage_executing_unkowntaskname_test() ->
    Result = mod_http_testhelper:process(
	       ["json"], 
	       #request{q=[{"tasks", <<"[{\"name\": \"unknown\"},{\"id\": \"123\"}]">>}]}
	      ),
    ?assertMatch({200, [{"Content-Type", "application/json"}], _Body}, Result),
    %%% extract and json decode result body
    DecodedBody = decode_json_body(Result),
    %%% test json object content
    ?assertMatch({struct, [{<<"success">>, false}|_T]}, DecodedBody).
    %%% TODO: test task errors

jsonpage_executing_missingtaskid_test() ->
    Result = mod_http_testhelper:process(
	       ["json"], 
	       #request{q=[{"tasks", <<"[{\"name\": \"unknown\"}]">>}]}
	      ),
    ?assertMatch({200, [{"Content-Type", "application/json"}], _Body}, Result).


%%%%%% TESTING HELPER FUNCTIONS

decode_json_body(HTTPReply) ->
    { _HTTPStatus, _HTTTPHeaders, Body } = HTTPReply,
    mochijson2:decode(Body).
