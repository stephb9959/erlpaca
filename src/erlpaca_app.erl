-module(erlpaca_app).
-behaviour(application).

-export([start/2,start/0]).
-export([stop/1]).

start(_Type, _Args) ->
	erlpaca_sup:start_link().

stop(_State) ->
	ok.

start() ->
	application:ensure_all_started(erlpaca).