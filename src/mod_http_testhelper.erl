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

process(["json"], _Request) ->
    {404, [], {xmlelement, "h1", [],
               [{xmlcdata, "404 Not implemented"}]}};

process([_LocalPath], _Request) ->
    {400, [], {xmlelement, "h1", [],
               [{xmlcdata, "400 Bad Request"}]}}.

%%%----------------------------------------------------------------------
%%% BEHAVIOUR CALLBACKS
%%%----------------------------------------------------------------------

start(_Host, _Opts) ->
    ok.

stop(_Host) ->
    ok. 
