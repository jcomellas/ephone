%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @author Paul Oliver <puzza007@gmail.com>
%%% @copyright (C) 2012 Juan Jose Comellas, Mahesh Paolini-Subramanya,
%%%                     Paul Oliver
%%% @doc
%%% Phone number parsing and validation.
%%% @end
%%%-------------------------------------------------------------------
-module(ephone).
-author('Juan Jose Comellas <juanjo@comellas.org>').

-behaviour(gen_server).

%% API
-export([start/0, stop/0]).
-export([get_env/0, get_env/1, get_env/2, set_env/2]).
-export([start_link/0, start_link/1]).
-export([country/1, country_codes/1, iso_code/1]).
-export([normalize_did/2, parse_did/2]).
-export([normalize_destination/2, parse_destination/2]).
-export([format/3]).
-export([clean_phone_number/1]).
-export([is_phone_number/1, is_country_code/1, is_iso_code/1]).
%% -export([parse/2, is_valid/1, normalize/1, format/2]).
-export([start_dial_rules/2, stop_dial_rules/1, ensure_dial_rules_started/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export_type([iso_code/0, country_code/0, area_code/0, phone_number/0, extension/0,
              phone_field/0, trunk_prefix/0, international_prefix/0,
              option/0, parse_option/0, billing_tag/0]).

-define(SERVER, ?MODULE).

-define(APP, ephone).
-define(BASENAME, "country_codes.json").
-define(DEFAULT_COUNTRY, "us").
-define(PHONE_NUMBER_REGEX, "^([\\+]{0,1}[0-9\\-_\\(\\)\\.\\,/\\s]+)(\\s*(x|xt|ex|ext|ext|extension|#|:)[\\.]{0,1}\\s*([0-9]+)){0,1}$").

-type iso_code()                                                    :: binary().
-type country_code()                                                :: binary().
-type area_code()                                                   :: binary().
-type phone_number()                                                :: binary().
-type extension()                                                   :: binary().
-type phone_field()                                                 :: {country_code, country_code()} | {area_code, area_code()} |
                                                                       {number, number()} | {extension, extension()}.
-type trunk_prefix()                                                :: binary().
-type international_prefix()                                        :: binary().
-type option()                                                      :: {filename, file:name()} | {format, json | csv}.
-type parse_option()                                                :: {iso_code, iso_code()} | {country_code, country_code()} |
                                                                       {area_code, area_code()}.
-type billing_tag()                                                 :: collect | domestic | emergency | international | local |
                                                                       mobile | operator | premium | toll_free.

-record(country, {
          iso_code = erlang:error({required, iso_code})             :: iso_code(),
          country_codes = erlang:error({required, country_code})    :: country_code() | [country_code()],
          country_short_code                                        :: [country_code()],
          country_name                                              :: binary(),
          trunk_prefix                                              :: trunk_prefix(),
          international_prefix                                      :: international_prefix()
         }).

-type country()                                                     :: #country{}.

-record(state, {
          default_iso_code                                          :: iso_code(),
          default_country_code                                      :: country_code(),
          default_area_code                                         :: area_code(),
          iso_codes                                                 :: dict(),
          country_codes                                             :: trie:trie(),
          phone_number_regex                                        :: re:mp()
         }).


%%%===================================================================
%%% API
%%%===================================================================

%% @doc Starts the gen_server that holds the country code mappings.
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Starts the gen_server that holds the country code mappings.
-spec start_link([option()]) -> {ok, pid()} | ignore | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

-spec country(iso_code() | country_code()) -> proplists:proplist() | undefined.
country(Code) ->
    gen_server:call(?SERVER, {country, to_lower(Code)}).

-spec country_codes(iso_code()) -> [country_code()].
country_codes(IsoCode) ->
    gen_server:call(?SERVER, {country_codes, to_lower(IsoCode)}).

-spec iso_code(country_code()) -> iso_code().
iso_code(CountryCode) ->
    gen_server:call(?SERVER, {iso_code, CountryCode}).

-spec normalize_did(phone_number(), [parse_option()]) -> {ok, phone_number()} | {error, Reason :: term()}.
normalize_did(PhoneNumber, Options) ->
    gen_server:call(?SERVER, {normalize_did, PhoneNumber, Options}).

-spec parse_did(phone_number(), [parse_option()]) -> {ok, proplists:proplist()} | {error, Reason :: term()}.
parse_did(PhoneNumber, Options) ->
    gen_server:call(?SERVER, {parse_did, PhoneNumber, Options}).

-spec normalize_destination(phone_number(), [parse_option()]) -> phone_number().
normalize_destination(PhoneNumber, Options) ->
    gen_server:call(?SERVER, {normalize_destination, PhoneNumber, Options}).

-spec parse_destination(phone_number(), [parse_option()]) -> {ok, proplists:proplist()} | {error, Reason :: term()}.
parse_destination(PhoneNumber, Options) ->
    gen_server:call(?SERVER, {parse_destination, PhoneNumber, Options}).

-spec format(Format :: binary() | string(), phone_number() | [phone_field()], [parse_option()]) -> iolist().
format(Format, PhoneNumber, Options) when is_binary(Format) ->
    gen_server:call(?SERVER, {format, Format, PhoneNumber, Options}).

-spec clean_phone_number(binary()) -> phone_number().
clean_phone_number(PhoneNumber) ->
    clean_phone_number(PhoneNumber, <<>>).

clean_phone_number(<<Digit, Tail/binary>>, Acc) when Digit >= $0, Digit =< $9 ->
    clean_phone_number(Tail, <<Acc/binary, Digit>>);
clean_phone_number(<<Char, Tail/binary>>, Acc) when Char =:= $+; Char =:= $-; Char =:= $.;
                                                    Char =:= $_; Char =:= $(; Char =:= $);
                                                    Char =:= $/; Char =:= $\s; Char =:= $, ->
    clean_phone_number(Tail, Acc);
clean_phone_number(_Tail, Acc) ->
    Acc.

-spec is_phone_number(binary()) -> boolean().
is_phone_number(PhoneNumber) ->
    gen_server:call(?SERVER, {is_phone_number, PhoneNumber}).

-spec is_country_code(binary()) -> boolean().
is_country_code(<<_Char, _Tail/binary>> = CountryCode) ->
    is_country_code_1(CountryCode, 0);
is_country_code(_Other) ->
    false.

is_country_code_1(<<Char, Tail/binary>>, Len) when Char >= $0, Char =< $9, Len =< 6 ->
    is_country_code_1(Tail, Len + 1);
is_country_code_1(<<>>, _Len) ->
    true;
is_country_code_1(_Other, _Len) ->
    false.

-spec is_iso_code(binary()) -> boolean().
is_iso_code(<<_Char, _Tail/binary>> = IsoCode) ->
    is_iso_code_1(IsoCode);
is_iso_code(_Other) ->
    false.

is_iso_code_1(<<Char, Tail/binary>>) when Char >= $a, Char =< $z ->
    is_iso_code_1(Tail);
is_iso_code_1(<<>>) ->
    true;
is_iso_code_1(_Other) ->
    false.

-spec start_dial_rules(iso_code(), country_code()) -> ephone_dial_rules_sup:start_dial_rules_ret().
start_dial_rules(IsoCode, CountryCode) ->
    ephone_dial_rules_sup:start_dial_rules(IsoCode, CountryCode).

-spec stop_dial_rules(iso_code() | pid()) -> ephone_dial_rules_sup:stop_dial_rules_ret().
stop_dial_rules(DialRulesRef) ->
    ephone_dial_rules_sup:stop_dial_rules(DialRulesRef).

-spec ensure_dial_rules_started(iso_code(), country_code()) -> ephone_dial_rules_sup:start_dial_rules_ret().
ensure_dial_rules_started(IsoCode, CountryCode) ->
    case start_dial_rules(IsoCode, CountryCode) of
        {error, {already_started, DialRulesRef}} ->
            {ok, DialRulesRef};
        Result ->
            Result
    end.

%% @doc Start the application and all its dependencies.
-spec start() -> ok.
start() ->
    start_deps(?APP).

%% @doc Stop the application and all its dependencies.
-spec stop() -> ok.
stop() ->
    stop_deps(?APP).

%% @doc Retrieve all key/value pairs in the env for the specified app.
-spec get_env() -> [{Key :: atom(), Value :: term()}].
get_env() ->
    application:get_all_env(?APP).

%% @doc The official way to get a value from the app's env.
%%      Will return the 'undefined' atom if that key is unset.
-spec get_env(Key :: atom()) -> term().
get_env(Key) ->
    get_env(Key, undefined).

%% @doc The official way to get a value from this application's env.
%%      Will return Default if that key is unset.
-spec get_env(Key :: atom(), Default :: term()) -> term().
get_env(Key, Default) ->
    case application:get_env(?APP, Key) of
        {ok, Value} ->
            Value;
        _ ->
            Default
    end.

%% @doc Sets the value of the configuration parameter Key for this application.
-spec set_env(Key :: atom(), Value :: term()) -> ok.
set_env(Key, Value) ->
    application:set_env(?APP, Key, Value).

-spec start_deps(App :: atom()) -> ok.
start_deps(App) ->
    application:load(App),
    {ok, Deps} = application:get_key(App, applications),
    lists:foreach(fun start_deps/1, Deps),
    start_app(App).

-spec start_app(App :: atom()) -> ok.
start_app(App) ->
    case application:start(App) of
        {error, {already_started, _}} -> ok;
        ok                            -> ok
    end.

-spec stop_deps(App :: atom()) -> ok.
stop_deps(App) ->
    stop_app(App),
    {ok, Deps} = application:get_key(App, applications),
    lists:foreach(fun stop_deps/1, lists:reverse(Deps)).

-spec stop_app(App :: atom()) -> ok.
stop_app(kernel) ->
    ok;
stop_app(stdlib) ->
    ok;
stop_app(App) ->
    case application:stop(App) of
        {error, {not_started, _}} -> ok;
        ok                        -> ok
    end.



%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initialize the gen_server that holds the country code mappings.
-spec init(Options :: list()) -> {ok, #state{}} | {stop, Reason ::  atom()} | {stop, timeout}.
init(Options) ->
    case load(country_codes_filename(Options), Options) of
        {ok, {IsoCodeDict, CountryCodeTrie}} ->
            Code = proplists:get_value(country, Options, <<?DEFAULT_COUNTRY>>),
            {IsoCode, CountryCode} = case is_iso_code(Code) of
                                         true ->
                                             {ok, Country} = dict:find(Code, IsoCodeDict),
                                             {Code, hd(Country#country.country_codes)};
                                         false ->
                                             {ok, Country} = trie:find(binary_to_list(Code), CountryCodeTrie),
                                             {Country#country.iso_code, Code}
                                     end,
            {ok, PhoneNumberRegex} = re:compile(?PHONE_NUMBER_REGEX),
            {ok, #state{
                    default_iso_code = IsoCode,
                    default_country_code = CountryCode,
                    iso_codes = IsoCodeDict,
                    country_codes = CountryCodeTrie,
                    phone_number_regex = PhoneNumberRegex
                   }};
        {error, Reason} ->
            {stop, Reason}
    end.


%% @private
%% @doc Handle call messages.
-spec handle_call(Request :: term(), From :: term(), #state{}) -> {reply, Reply :: term(), #state{}}.
%% country/1 callback
handle_call({country, Code}, _From, State) ->
    FindCountry = case is_iso_code(Code) of
                      true  ->
                          fun () -> dict:find(Code, State#state.iso_codes) end;
                      false ->
                          case is_country_code(Code) of
                              true ->
                                  fun () -> trie:find(binary_to_list(Code), State#state.country_codes) end;
                              false ->
                                  fun () -> error end
                          end
                  end,
    Reply = case FindCountry() of
                {ok, Country} ->
                    [{iso_code, Country#country.iso_code},
                     {country_codes, Country#country.country_codes},
                     {country_short_code, Country#country.country_short_code},
                     {country_name, Country#country.country_name},
                     {trunk_prefix, Country#country.trunk_prefix},
                     {international_prefix, Country#country.international_prefix}];
                error ->
                    undefined
            end,
    {reply, Reply, State};
%% country_code/1 callback
handle_call({country_codes, IsoCode}, _From, State) ->
    Reply = case dict:find(IsoCode, State#state.iso_codes) of
                {ok, Country} -> Country#country.country_codes;
                error         -> undefined
            end,
    {reply, Reply, State};
%% iso_code/1 callback
handle_call({iso_code, CountryCode}, _From, State) ->
    Reply = case trie:find(binary_to_list(CountryCode), State#state.country_codes) of
                {ok, Country} -> Country#country.iso_code;
                error         -> undefined
            end,
    {reply, Reply, State};
%% normalize_did/1 callback
handle_call({normalize_did, PhoneNumber, Options}, _From, State) when is_binary(PhoneNumber) ->
    {reply, normalize_did_internal(PhoneNumber, Options, State), State};
%% split_country_code/1 callback
handle_call({split_country_code, PhoneNumber}, _From, State) when is_binary(PhoneNumber) ->
    {reply, split_country_code_internal(PhoneNumber, State), State};
%% split_extension/1 callback
handle_call({split_extension, PhoneNumber}, _From, State) when is_binary(PhoneNumber) ->
    {reply, split_extension_internal(PhoneNumber, State), State};
%% parse_did/2 callback
handle_call({parse_did, PhoneNumber, Options}, _From, State) ->
    {reply, parse_did_internal(PhoneNumber, Options, State), State};
%% normalize_destination/1 callback
handle_call({normalize_destination, PhoneNumber, Options}, _From, State) when is_binary(PhoneNumber) ->
    {reply, normalize_destination_internal(PhoneNumber, Options), State};
%% parse_destination/2 callback
handle_call({parse_destination, PhoneNumber, Options}, _From, State) ->
    {reply, parse_destination_internal(PhoneNumber, Options, State), State};
%% format/3 callback
handle_call({format, Format, PhoneNumber, Options}, _From, State) ->
    {reply, format_internal(Format, PhoneNumber, Options, State), State};
%% is_phone_number/1 callback
handle_call({is_phone_number, PhoneNumber}, _From, State) ->
    {reply, is_phone_number_internal(PhoneNumber, State), State};
handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.


%% @private
%% @doc Handle cast messages.
-spec handle_cast(Msg :: term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.


%% @private
%% @doc Handle dialplan fetch requests from FreeSWITCH.
-spec handle_info(Msg :: term(), #state{}) -> {'noreply', #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.


%% @private
%% @doc This function is called by a gen_server when it is about to
%%      terminate. It should be the opposite of Module:init/1 and do any
%%      necessary cleaning up. When it returns, the gen_server terminates
%%      with Reason. The return value is ignored.
-spec terminate(Reason :: term(), #state{}) -> 'ok'.
terminate(_Reason, _State) ->
    ok.


%% @private
%% @doc Convert process state when code is changed
-spec code_change(OldVsn :: term(), #state{}, Extra :: term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec country_codes_filename([option()]) -> file:name().
country_codes_filename(Options) ->
    case proplists:get_value(filename, Options) of
        Str when is_list(Str) ->
            Str;
        undefined ->
            filename:join([country_codes_dir(), ?BASENAME])
    end.


-spec load(file:name(), [option()]) -> {ok, {IsoCodes :: dict(), CountryCodes :: trie:trie()}} | {error, Reason :: term()}.
load(Filename, Options) ->
    case file:read_file(Filename) of
        {ok, Bin} ->
            decode(Bin, Options);
        {error, Posix} ->
            {error, {Posix, Filename}}
    end.

-spec decode(binary(), [option()]) -> {ok, {IsoCodes :: dict(), CountryCodes :: trie:trie()}} | {error, Reason :: term()}.
decode(Bin, Options) ->
    case proplists:get_value(format, Options, json) of
        json ->
            decode_json(Bin);
        Format ->
            {error, {not_implemented, Format}}
    end.


-spec decode_json(binary()) -> {ok, {IsoCodes :: dict(), CountryCodes :: trie:trie()}} | no_return().
decode_json(Bin) ->
    JsonTerm = jsx:decode(Bin),
    {ok, decode_json_countries(JsonTerm)}.


-spec decode_json_countries(jsx:json_term()) -> {IsoCodes :: dict(), CountryCodes :: trie:trie()}.
decode_json_countries(JsonTerm) ->
    decode_json_countries(JsonTerm, {dict:new(), trie:new()}).

-spec decode_json_countries(jsx:json_term(), Acc :: {IsoCodes :: dict(), CountryCodes :: trie:trie()}) ->
                                   {IsoCodes :: dict(), CountryCodes :: trie:trie()}.
decode_json_countries([JsonTerm | Tail], {IsoCodeDict, CountryCodeTrie}) ->
    IsoCode = kvc:path(<<"iso_code">>, JsonTerm),
    CountryCodes = case kvc:path(<<"country_codes">>, JsonTerm) of
                       CountryCode when is_binary(CountryCode) -> [CountryCode];
                       List when is_list(List)                 -> List
                   end,
    CountryShortCode = case kvc:path(<<"country_short_code">>, JsonTerm) of
                       Code when is_binary(Code) -> [Code];
                       _ -> []

                   end,
    Country = #country{
                 country_codes = CountryCodes,
                 iso_code = IsoCode,
                 country_short_code = CountryShortCode,
                 country_name = kvc:path(<<"country_name">>, JsonTerm),
                 trunk_prefix = kvc:path(<<"trunk_prefix">>, JsonTerm),
                 international_prefix = kvc:path(<<"international_prefix">>, JsonTerm)
                },
    NewIsoCodeDict = dict:store(IsoCode, Country, IsoCodeDict),
    NewCountryCodeTrie = lists:foldl(fun (Code, Trie) -> trie:store(binary_to_list(Code), Country, Trie) end,
                                     CountryCodeTrie, CountryCodes),
    decode_json_countries(Tail, {NewIsoCodeDict, NewCountryCodeTrie});
decode_json_countries([], Acc) ->
    Acc.


-spec normalize_did_internal(phone_number(), [parse_option()], #state{}) -> {ok, phone_number()} | {error, Reason :: term()}.
normalize_did_internal(<<$+, PhoneNumber/binary>> = FullPhoneNumber, _Options, _State) ->
    case clean_phone_number(PhoneNumber) of
        <<>> ->
            {error, {invalid_did, FullPhoneNumber}};
        CleanNumber ->
            {ok, <<$+, CleanNumber/binary>>}
    end;
normalize_did_internal(PhoneNumber, Options, State) ->
    CountryCode = proplists:get_value(country_code, Options, State#state.default_country_code),
    CountryCodeLen = byte_size(CountryCode),
    case clean_phone_number(PhoneNumber) of
        <<>> ->
            {error, {invalid_did, PhoneNumber}};
        <<CountryCode:CountryCodeLen/binary, _DomesticNumber/binary>> = CleanNumber ->
            {ok, <<$+, CleanNumber/binary>>};
        CleanNumber ->
            {ok, <<$+, CountryCode/binary, CleanNumber/binary>>}
    end.

-spec parse_did_internal(phone_number(), [parse_option()], #state{}) -> {ok, proplists:proplist()} | {error, Reason :: term()}.
parse_did_internal(FullPhoneNumber, Options, State) ->
    {PhoneNumber, Extension} = split_extension_internal(FullPhoneNumber, State),
    case normalize_did_internal(PhoneNumber, Options, State) of
        {ok, NormalizedNumber} ->
            {CountryCode, DomesticNumber} = split_country_code_internal(NormalizedNumber, State),
            Tail = case Extension of
                       undefined -> [];
                       _         -> [{extension, Extension}]
                   end,
            {ok, [{country_code, CountryCode}, {phone_number, DomesticNumber} | Tail]};
        Error ->
            Error
    end.


%% @doc Remove non-digit characters from a dialed (destination) phone number.
-spec normalize_destination_internal(phone_number(), [parse_option()]) -> phone_number().
normalize_destination_internal(PhoneNumber, _Options) when is_binary(PhoneNumber) ->
    clean_phone_number(PhoneNumber).


-spec parse_destination_internal(phone_number(), [parse_option()], #state{}) ->
                                        {ok, proplists:proplist()} | {error, Reason :: term()}.
parse_destination_internal(FullPhoneNumber, Options, State) ->
    {PhoneNumber, Extension} = split_extension_internal(FullPhoneNumber, State),
    NormalizedNumber = normalize_destination_internal(PhoneNumber, Options),
    IsoCode = proplists:get_value(iso_code, Options, State#state.default_iso_code),
    case dict:find(to_lower(IsoCode), State#state.iso_codes) of
        {ok, Country} ->
            case ephone:ensure_dial_rules_started(IsoCode, Country#country.iso_code) of
                {ok, DialRulesRef} ->
                    case ephone_dial_rules:parse_destination(DialRulesRef, NormalizedNumber, Options) of
                        {ok, _ParsedDestination} = Result when Extension =:= undefined ->
                            Result;
                        {ok, ParsedDestination} ->
                            {ok, ParsedDestination ++ [{extension, Extension}]};
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end;
        error ->
            {error, {invalid_iso_code, IsoCode}}
    end.


format_internal(Format, PhoneNumber, Options, State) ->
    ParsedNumber = case PhoneNumber of
                       <<$+, _Tail/binary>> ->
                           parse_did_internal(PhoneNumber, Options, State);
                       _ when is_binary(PhoneNumber) ->
                           parse_destination_internal(PhoneNumber, Options, State);
                       [{_Key, _Value} | _Tail] ->
                           lists:foldl(fun ({Key1, _Value1} = Tuple, Acc) ->
                                               lists:keystore(Key1, 1, Acc, Tuple)
                                       end, PhoneNumber, Options)
                   end,
    if
        is_binary(Format) ->
            format_binary_internal(Format, ParsedNumber, State, []);
        true ->
            format_list_internal(Format, ParsedNumber, State, [])
    end.


format_binary_internal(<<$%, EscapeCode, Tail/binary>>, ParsedNumber, State, Acc) ->
    format_binary_internal(Tail, ParsedNumber, State, [format_field(EscapeCode, ParsedNumber, State) | Acc]);
format_binary_internal(<<Char, Tail/binary>>, ParsedNumber, State, Acc) ->
    format_binary_internal(Tail, ParsedNumber, State, [Char | Acc]);
format_binary_internal(<<>>, _ParsedNumber, _State, Acc) ->
    lists:reverse(Acc).

format_list_internal([$%, EscapeCode | Tail], ParsedNumber, State, Acc) ->
    format_list_internal(Tail, ParsedNumber, State, [format_field(EscapeCode, ParsedNumber, State) | Acc]);
format_list_internal([Char | Tail], ParsedNumber, State, Acc) ->
    format_list_internal(Tail, ParsedNumber, State, [Char | Acc]);
format_list_internal([], _ParsedNumber, _State, Acc) ->
    lists:reverse(Acc).

format_field($C, ParsedNumber, State) ->
    %% Country code with leading '+'
    [$+, proplists:get_value(country_code, ParsedNumber, State#state.default_country_code)];
format_field($c, ParsedNumber, State) ->
    %% Country code
    proplists:get_value(country_code, ParsedNumber, State#state.default_country_code);
format_field($A, ParsedNumber, State) ->
    %% Area code with leading '0'
    case proplists:get_value(area_code, ParsedNumber, State#state.default_area_code) of
        AreaCode when byte_size(AreaCode) > 0 -> [$0, AreaCode];
        undefined                             -> <<>>
    end;
format_field($a, ParsedNumber, State) ->
    %% Area code
    proplists:get_value(area_code, ParsedNumber, State#state.default_area_code);
format_field($n, ParsedNumber, _State) ->
    %% Number
    proplists:get_value(number, ParsedNumber, <<>>);
format_field($x, ParsedNumber, _State) ->
    %% Extension
    proplists:get_value(extension, ParsedNumber, <<>>);
format_field($X, ParsedNumber, _State) ->
    %% Extension with leading 'x'
    case proplists:get_value(extension, ParsedNumber) of
        Extension when byte_size(Extension) > 0 -> [$x, Extension];
        undefined                               -> <<>>
    end.


-spec is_phone_number_internal(binary(), #state{}) -> boolean().
is_phone_number_internal(PhoneNumber, #state{phone_number_regex = PhoneNumberRegex}) ->
    case re:run(PhoneNumber, PhoneNumberRegex) of
        {match, _} -> true;
        nomatch    -> false
    end.


-spec split_country_code_internal(phone_number(), #state{}) -> {country_code() | undefined, phone_number()}.
split_country_code_internal(<<$+, PhoneNumber/binary>>, State) ->
    split_country_code_internal(PhoneNumber, State);
split_country_code_internal(PhoneNumber, State) ->
    CleanNumber = clean_phone_number(PhoneNumber),
    case trie:find_prefix_longest(binary_to_list(CleanNumber), State#state.country_codes) of
        {ok, CountryCodeStr, Country} ->
            CountryCode = country_code(list_to_binary(CountryCodeStr), Country),
            CountryCodeLen = byte_size(CountryCode),
            <<CountryCode:CountryCodeLen/binary, DomesticNumber/binary>> = CleanNumber,
            {CountryCode, DomesticNumber};
        error ->
            {undefined, PhoneNumber}
    end.

-spec country_code(country_code(), country()) -> country_code().
country_code(Code, #country{country_short_code = []}) ->
    Code;
country_code(Code, #country{country_short_code = [ShortCode]}) ->
    S = byte_size(ShortCode),
    case Code of
        <<ShortCode:S/binary, _/binary>> -> ShortCode;
        _ -> Code
    end.

-spec split_extension_internal(phone_number(), #state{}) -> {phone_number(), extension() | undefined}.
split_extension_internal(FullPhoneNumber, State) ->
    case re:run(FullPhoneNumber, State#state.phone_number_regex) of
        {match, [_FullPosLen, PhonePosLen]} ->
            %% Phone number without an extension: the phone number is in the first match group
            {binary_part(FullPhoneNumber, PhonePosLen), undefined};
        {match, [_FullPosLen, PhonePosLen, _Group2, _Group3, ExtPosLen]} ->
            %% Phone number with an extension: the phone number is in the first match group
            %% and the extension in the 4th (last) one
            {binary_part(FullPhoneNumber, PhonePosLen), binary_part(FullPhoneNumber, ExtPosLen)};
        nomatch ->
            {FullPhoneNumber, undefined}
    end.


-spec country_codes_dir() -> file:name().
country_codes_dir() ->
    case code:priv_dir(?APP) of
        Dir when is_list(Dir) ->
            Dir;
        _Error ->
            "priv"
    end.

%% @doc Convert all the characters in a binary to lowercase.
-spec to_lower(binary()) -> binary().
to_lower(Str) ->
    to_lower(Str, <<>>).

to_lower(<<Char, Tail/binary>>, Acc) when Char >= $A, Char =< $Z ->
    Lower = Char - $A + $a,
    to_lower(Tail, <<Acc/binary, Lower>>);
to_lower(<<Char, Tail/binary>>, Acc) ->
    to_lower(Tail, <<Acc/binary, Char>>);
to_lower(<<>>, Acc) ->
    Acc.
