%%% Iterative Development Console Helper
-module(itr).

-compile(export_all).

r() ->
    make:all(),
    [ covercompile_and_reload(X) || X <- [mod_http_testhelper, mod_http_testhelper_tests]],
    eunit:test(mod_http_testhelper,[verbose]),
    [ coversave(X) || X <- [mod_http_testhelper, mod_http_testhelper_tests]].        

covercompile_and_reload(X) ->
    c:l(X),
    cover:compile("src/"++atom_to_list(X)++".erl", [{i, "include"},{i, "/usr/lib/erlang/lib"}]),
    cover:compile("tests/"++atom_to_list(X)++".erl", [{i, "include"},{i, "/usr/lib/erlang/lib"}]).
%%%    cover:compile_beam(X).  %%% SIMPLER BUT CANT REPORT TESTS FILE

coversave(X) ->
    cover:analyse_to_file(X,"tmp/"++atom_to_list(X)++"_coverage.html",[html]).
