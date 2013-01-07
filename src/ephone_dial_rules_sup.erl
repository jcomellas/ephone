%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @author Paul Oliver <puzza007@gmail.com>
%%% @copyright (C) 2012 Juan Jose Comellas, Mahesh Paolini-Subramanya, Paul Oliver
%%% @doc
%%% Country dialing rules supervisor.
%%% @end
%%%-------------------------------------------------------------------
-module(ephone_dial_rules_sup).
-author('Juan Jose Comellas <juanjo@comellas.org>').

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_dial_rules/2, stop_dial_rules/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(WORKER(Id, Restart, Module, Args), {Id, {Module, start_link, Args}, Restart, 5000, worker, [Module]}).

-type startlink_err()                           :: {'already_started', pid()} | 'shutdown' | term().
-type startlink_ret()                           :: {'ok', pid()} | 'ignore' | {'error', startlink_err()}.
-type start_dial_rules_ret()                    :: supervisor:startchild_ret().
-type stop_dial_rules_ret()                     :: ok | {error, Reason :: term()}.

-export_type([startlink_err/0, startlink_ret/0, start_dial_rules_ret/0, stop_dial_rules_ret/0]).


-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


-spec start_dial_rules(ephone:iso_code(), ephone:country_code()) -> start_dial_rules_ret().
start_dial_rules(IsoCode, CountryCodes) ->
    supervisor:start_child(?SERVER, [IsoCode, CountryCodes]).


-spec stop_dial_rules(ephone:iso_code() | pid()) -> ok | {error, Reason :: term()}.
stop_dial_rules(DialRulesRef) ->
    case DialRulesRef of
        DialRulesPid when is_pid(DialRulesPid) ->
            supervisor:terminate_child(?SERVER, DialRulesPid);
        IsoCode when is_binary(IsoCode) ->
            case whereis(ephone_dial_rules:registered_name(IsoCode)) of
                DialRulesPid when is_pid(DialRulesPid) ->
                    supervisor:terminate_child(?SERVER, DialRulesPid);
                undefined ->
                    {error, {not_found, IsoCode}}
            end
    end.


-spec init(Args :: term()) -> {ok, {{RestartStrategy :: supervisor:strategy(), MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
                                    [ChildSpec :: supervisor:child_spec()]}}.
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 100,
    MaxSecondsBetweenRestarts = 60,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags, [
                     ?WORKER(ephone_dial_rules, transient, ephone_dial_rules, [])
                    ]}}.
