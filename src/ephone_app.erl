%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @author Paul Oliver <puzza007@gmail.com>
%%% @copyright (C) 2012 Juan Jose Comellas, Mahesh Paolini-Subramanya,
%%%                     Paul Oliver
%%% @doc
%%% Phone parsing and validation application.
%%% @end
%%%-------------------------------------------------------------------
-module(ephone_app).
-author('Juan Jose Comellas <juanjo@comellas.org>').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-spec start(StartType :: normal | {takeover, node()} | {failover, node()}, StartArgs :: [term()]) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    ephone_sup:start_link().

stop(_State) ->
    ok.
