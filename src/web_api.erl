%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Jun 2020 3:59 p.m.
%%%-------------------------------------------------------------------
-module(web_api).
-author("stephb").

%% API
-export([init/0]).

init()->
	Dispatch = cowboy_router:compile([
		{'_', [{"/api/v1/:device_type/:device_number/", alpaca_handler, []}]}
		]),
		{ok, _} = cowboy:start_clear(my_http_listener,
		[{port, application:get_env(cowboy,port,8080)}],
		#{env => #{dispatch => Dispatch}}
	), ok.