%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @author Paul Oliver <puzza007@gmail.com>
%%% @copyright (C) 2012 Juan Jose Comellas, Mahesh Paolini-Subramanya,
%%%                     Paul Oliver
%%% @doc
%%% Phone parsing and validation supervisor.
%%% @end
%%%-------------------------------------------------------------------
-module(ephone_sup).
-author('Juan Jose Comellas <juanjo@comellas.org>').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(SUPERVISOR(Id, Module, Args), {Id, {Module, start_link, Args}, permanent, 5000, supervisor, [Module]}).
-define(WORKER(Id, Module, Args), {Id, {Module, start_link, Args}, permanent, 5000, worker, [Module]}).

-type startlink_err()                       :: {'already_started', pid()} | 'shutdown' | term().
-type startlink_ret()                       :: {'ok', pid()} | 'ignore' | {'error', startlink_err()}.


-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init(Args :: term()) -> {ok, {{RestartStrategy :: supervisor:strategy(), MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
                                    [ChildSpec :: supervisor:child_spec()]}}.
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 100,
    MaxSecondsBetweenRestarts = 60,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags, [
                     ?WORKER(ephone, ephone, [])
                    ]}}.
