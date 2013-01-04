%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @author Paul Oliver <puzza007@gmail.com>
%%% @copyright (C) 2012 Juan Jose Comellas, Mahesh Paolini-Subramanya,
%%%                     Paul Oliver
%%% @doc
%%% Country dialing rules.
%%% @end
%%%-------------------------------------------------------------------
-module(ephone_dial_rules).

-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([registered_name/1]).
-export([iso_code/1, international_access_code/1, trunk_code/1]).
-export([parse_destination/3]).
-export([reload/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(APP, ephone).
-define(REGISTERED_NAME_PREFIX, "ephone.").
-define(DIAL_RULES_PREFIX, "dial_rules_").
-define(JSON_EXTENSION, ".json").
-define(TRUNK_CODE, "trunk_code").
-define(INTERNATIONAL_ACCESS_CODE, "international_access_code").
-define(DIAL_RULES, "dial_rules").
-define(BILLING_TAGS, "billing_tags").
-define(REGEXP, "regexp").
-define(MATCH_ACTION, "match_action").

-type server_ref()                                                  :: atom() | {atom(), node()} |
                                                                       pid() | ephone:iso_code().
-type match_action()                                                :: return | reparse_without_international_access_code.

-record(dial_rule, {
          billing_tags                                              :: ephone:billing_tag() | [ephone:billing_tag()],
          regexp                                                    :: binary(),
          mp                                                        :: re:mp(),         %% compiled regular expression
          match_action = return                                     :: match_action()
         }).

-record(state, {
          iso_code = erlang:error({required, iso_code})             :: ephone:iso_code(),
          country_code = erlang:error({required, country_code})     :: ephone:country_code(),
          default_area_code                                         :: ephone:area_code(),
          trunk_code                                                :: ephone:dialing_prefix(),
          international_access_code                                 :: ephone:dialing_prefix(),
          dial_rules = []                                           :: [#dial_rule{}]
         }).


%%%===================================================================
%%% API
%%%===================================================================

%% @doc Starts the dialing rules for a country.
-spec start_link(ephone:iso_code()) -> {ok, pid()} | ignore | {error, Reason :: term()}.
start_link(IsoCode) ->
    gen_server:start_link({local, registered_name(IsoCode)}, ?MODULE, [IsoCode], []).

%% @doc Return the name used to register a dialing rule.
-spec registered_name(ephone:iso_code()) -> atom().
registered_name(IsoCode) when is_binary(IsoCode) ->
    binary_to_atom(<<?REGISTERED_NAME_PREFIX, IsoCode/binary>>, utf8).

-spec iso_code(server_ref()) -> ephone:iso_code().
iso_code(ServerRef) ->
    call(ServerRef, iso_code).

-spec trunk_code(server_ref()) -> ephone:dialing_prefix().
trunk_code(ServerRef) ->
    call(ServerRef, trunk_code).

-spec international_access_code(server_ref()) -> ephone:dialing_prefix().
international_access_code(ServerRef) ->
    call(ServerRef, international_access_code).

-spec parse_destination(server_ref(), Destination :: ephone:phone_number(), [ephone:parse_option()]) ->
                               {ok, proplists:proplist()} | {error, Reason :: term()}.
parse_destination(ServerRef, Destination, Options) ->
    call(ServerRef, {parse_destination, Destination, Options}).

-spec reload(server_ref()) -> ok | {error, Reason :: term()}.
reload(ServerRef) ->
    call(ServerRef, reload).


call(IsoCode, Request) when is_binary(IsoCode) ->
    gen_server:call(registered_name(IsoCode), Request);
call(ServerRef, Request) ->
    gen_server:call(ServerRef, Request).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the dial rules.
-spec init(Args :: list()) -> {ok, #state{}}.
init([IsoCode, CountryCode]) ->
    {ok, {DomesticDialingPrefix, InternationalAccessCode, DialRules}} = load_country_rules(IsoCode),
    {ok, #state{
            iso_code = IsoCode,
            country_code = CountryCode,
            trunk_code = DomesticDialingPrefix,
            international_access_code = InternationalAccessCode,
            dial_rules = DialRules
           }}.


%% @private
%% @doc Handle call messages.
-spec handle_call(Request :: term(), From :: term(), #state{}) -> {reply, Reply :: term(), #state{}} |
                                                                  {reply, Reply :: term(), #state{}, timeout() | 'hibernate'} |
                                                                  {noreply, #state{}} |
                                                                  {noreply, #state{}, timeout() | 'hibernate'} |
                                                                  {stop, Reason :: term(), Reply :: term(), #state{}} |
                                                                  {stop, Reason :: term(), #state{}}.
%% iso_code/1 callback
handle_call(iso_code, _From, State) ->
    {reply, State#state.iso_code, State};
%% trunk_code/1 callback
handle_call(trunk_code, _From, State) ->
    {reply, State#state.trunk_code, State};
%% international_access_code/1 callback
handle_call(international_access_code, _From, State) ->
    {reply, State#state.international_access_code, State};
%% parse_destination/2 callback
handle_call({parse_destination, Destination, Options}, _From, State) ->
    {reply, parse_destination_internal(Destination, Options, State), State};
%% reload/1 callback
handle_call(reload, _From, State) ->
    case load_country_rules(State#state.iso_code) of
        {ok, {DomesticDialingPrefix, InternationalAccessCode, DialRules}} ->
            NewState = State#state{
                         trunk_code = DomesticDialingPrefix,
                         international_access_code = InternationalAccessCode,
                         dial_rules = DialRules
                        },
            {reply, ok, NewState};
        Error ->
            {reply, Error, State}
    end;
handle_call(_Request, _From, State) ->
    Reply = {error, {not_implemented, _Request}},
    {reply, Reply, State}.


%% @private
%% @doc Handle cast messages.
-spec handle_cast(Request :: term(), #state{}) -> {noreply, #state{}} |
                                                  {noreply, #state{}, timeout() | 'hibernate'} |
                                                  {stop, Reason :: term(), #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.


%% @private
%% @doc Handle all non call/cast messages.
-spec handle_info(Request :: term(), #state{}) -> {noreply, #state{}} |
                                                  {noreply, #state{}, timeout() | 'hibernate'} |
                                                  {stop, Reason :: term(), #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.


%% @private
%% @doc This function is called by a gen_server when it is about to terminate.
%%      It should be the opposite of Module:init/1 and do any necessary
%%      cleaning up. When it returns, the gen_server terminates with Reason.
%%      The return value is ignored.
-spec terminate(Reason :: term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.


%% @private
%% @doc Convert process state when code is changed.
-spec code_change(OldVsn :: term(), #state{}, Extra :: term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec load_country_rules(ephone:iso_code()) -> {ok, [#dial_rule{}]} | {error, Reason :: term()}.
load_country_rules(IsoCode) ->
    Filename = filename:join([code:priv_dir(?APP),
                              ?DIAL_RULES_PREFIX ++ binary_to_list(IsoCode) ++ ?JSON_EXTENSION]),
    case file:read_file(Filename) of
        {ok, JsonText} ->
            JsonTerm = jsx:decode(JsonText),
            case {kvc:path(<<?TRUNK_CODE>>, JsonTerm),
                  kvc:path(<<?INTERNATIONAL_ACCESS_CODE>>, JsonTerm),
                  decode_country_rules(kvc:path(<<?DIAL_RULES>>, JsonTerm))} of
                {DomesticDialingPrefix, InternationalAccessCode, DialRules} = CountryRules
                  when is_binary(DomesticDialingPrefix), is_binary(InternationalAccessCode), is_list(DialRules) ->
                    {ok, CountryRules};
                _ ->
                    {error, {invalid_country_rules, Filename}}
            end;
        {error, Posix} ->
            {error, {Posix, Filename}}
    end.

-spec decode_country_rules(Rules :: jsx:json_term()) -> {ok, [#dial_rule{}]} | {error, Reason :: term()}.
decode_country_rules(Rules) ->
    try
        decode_country_rules(Rules, [])
    catch
        throw:Error ->
            Error
    end.

decode_country_rules([JsonTerm | Tail], Acc) ->
    Regexp = kvc:path(<<?REGEXP>>, JsonTerm),
    case re:compile(Regexp) of
        {ok, MP} ->
            DialRule = #dial_rule{
                          billing_tags = [binary_to_billing_tag(Bin) || Bin <- kvc:path(<<?BILLING_TAGS>>, JsonTerm)],
                          regexp = Regexp,
                          mp = MP,
                          match_action = binary_to_match_action(kvc:path(<<?MATCH_ACTION>>, JsonTerm), return)
                         },
            decode_country_rules(Tail, [DialRule | Acc]);
        Error ->
            erlang:throw(Error)
    end;
decode_country_rules([], Acc) ->
    {ok, lists:reverse(Acc)}.


-spec binary_to_billing_tag(binary()) -> ephone:billing_tag().
binary_to_billing_tag(Bin) when is_binary(Bin) ->
    binary_to_atom(Bin, utf8).


-spec binary_to_match_action(binary() | [], match_action()) -> match_action().
binary_to_match_action([], DefaultMatchAction) ->
    DefaultMatchAction;
binary_to_match_action(<<"reparse_without_international_access_code">>, _DefaultMatchAction) ->
    reparse_without_international_access_code;
binary_to_match_action(<<"return">>, _DefaultMatchAction) ->
    return.


-spec parse_destination_internal(ephone:phone_number(), [ephone:parse_option()], #state{}) ->
                                        {ok, proplists:proplist()} | {error, Reason :: term()}.
parse_destination_internal(Destination, Options,
                           #state{international_access_code = InternationalAccessCode, dial_rules = DialRules} = State) ->
    case get_billing_tags(Destination, DialRules) of
        {return, Destination, BillingTags} ->
            {ok, [{phone_number, Destination}, {billing_tags, BillingTags}]};
        {reparse_without_international_access_code, Destination, _BillingTags} ->
            Len = size(InternationalAccessCode),
            <<_InternationalAccessCode:Len/binary, DomesticNumber/binary>> = Destination,
            parse_destination_internal(DomesticNumber, Options, State)
    end.

get_billing_tags(Destination, [#dial_rule{billing_tags = BillingTags, mp = MP, match_action = MatchAction} | Tail]) ->
    case re:run(Destination, MP) of
        {match, _Match} ->
            %% lager:debug("Destination ~s matched for billing tags '~p'~n", [Destination, BillingTags]),
            {MatchAction, Destination, BillingTags};
        nomatch ->
            %% lager:debug("Destination ~s did not match for billing tags '~p'~n", [Destination, BillingTags]),
            get_billing_tags(Destination, Tail)
    end;
get_billing_tags(_Destination, []) ->
    undefined.
