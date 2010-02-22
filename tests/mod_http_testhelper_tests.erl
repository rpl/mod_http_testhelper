-module(mod_http_testhelper_tests).

-include("eunit/include/eunit.hrl").

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_http.hrl").

rootpage_test() ->
    Result = mod_http_testhelper:process([], #request{q={nokey,[]}}),
    ?assertMatch({xmlelement, "html", _Attrs , _Childs}, Result).

jsonpage_without_queryparams_test() ->
    Result = mod_http_testhelper:process(["json"], #request{q=[{nokey,[]}]}),
    ?assertMatch({504, _Headers, _Body}, Result).
    
nonexistent_paths_test() ->
    Result = mod_http_testhelper:process(["nonexistent"], #request{}),
    ?assertMatch({400, _Headers, _Body}, Result).
    
jsonpage_executing_jsoninvalid_queryparams_test() ->
    Result = mod_http_testhelper:process(["json"], #request{q=[{invalid_param, "value"}]}),
%%    ?debugVal(Result),
    ?assertMatch({200, [{"Content-Type", "application/json"}], _Body}, Result).
    %%% assertMatch on Body

jsonpage_executing_unkowntaskname_test() ->
    Result = mod_http_testhelper:process(
	       ["json"], 
	       #request{q=[{"tasks", <<"[{\"name\": \"unknown\"},{\"id\", \":123\"}]">>}]}
	      ),
    ?assertMatch({200, [{"Content-Type", "application/json"}], _Body}, Result).

jsonpage_executing_missingtaskid_test() ->
    Result = mod_http_testhelper:process(
	       ["json"], 
	       #request{q=[{"tasks", <<"[{\"name\": \"unknown\"}]">>}]}
	      ),
    ?assertMatch({200, [{"Content-Type", "application/json"}], _Body}, Result).

