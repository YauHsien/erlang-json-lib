%%%-------------------------------------------------------------------
%% @doc ecjapp public API
%% @end
%%%-------------------------------------------------------------------

-module(ecjapp_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ecjapp_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
