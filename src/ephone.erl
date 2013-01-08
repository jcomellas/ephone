%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @author Paul Oliver <puzza007@gmail.com>
%%% @copyright (C) 2012 Juan Jose Comellas, Mahesh Paolini-Subramanya,
%%%                     Paul Oliver
%%% @doc
%%% Phone parsing and validation.
%%% @end
%%%-------------------------------------------------------------------
-module(ephone).
-author('Juan Jose Comellas <juanjo@comellas.org>').

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).
-export([country/1, country_codes/1, iso_code/1]).
-export([normalize_did/2, parse_did/2]).
-export([normalize_destination/2, parse_destination/2]).
-export([format/3]).
-export([clean_phone_number/1]).
-export([is_country_code/1, is_iso_code/1]).
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
-define(EXTENSION_REGEXP, "\s*(ext|ex|x|xt|#|:)+[^0-9]*\\(*([-0-9]+)\\)*#?$").
-define(PHONE_CLEANUP_REGEXP, "[^0-9]*$").

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
          country_name                                              :: binary(),
          trunk_prefix                                              :: trunk_prefix(),
          international_prefix                                      :: international_prefix()
         }).

-record(state, {
          default_iso_code                                          :: iso_code(),
          default_country_code                                      :: country_code(),
          default_area_code                                         :: area_code(),
          iso_codes                                                 :: dict(),
          country_codes                                             :: trie:trie(),
          extension_regexp                                          :: re:mp(),
          phone_cleanup_regexp                                      :: re:mp()
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
    gen_server:call(?SERVER, {country, Code}).

-spec country_codes(iso_code()) -> [country_code()].
country_codes(IsoCode) ->
    gen_server:call(?SERVER, {country_codes, IsoCode}).

-spec iso_code(country_code()) -> iso_code().
iso_code(CountryCode) ->
    gen_server:call(?SERVER, {iso_code, CountryCode}).

-spec normalize_did(phone_number(), [parse_option()]) -> phone_number().
normalize_did(PhoneNumber, Options) ->
    gen_server:call(?SERVER, {normalize_did, PhoneNumber, Options}).

-spec parse_did(phone_number(), [parse_option()]) -> proplists:proplist().
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

-spec clean_phone_number(phone_number()) -> phone_number().
clean_phone_number(PhoneNumber) ->
    << <<Digit>> || <<Digit>> <= PhoneNumber, Digit >= $0, Digit =< $9 >>.

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
            {ok, ExtensionMP} = re:compile(?EXTENSION_REGEXP),
            {ok, CleanupMP} = re:compile(?PHONE_CLEANUP_REGEXP),
            {ok, #state{
                    default_iso_code = IsoCode,
                    default_country_code = CountryCode,
                    iso_codes = IsoCodeDict,
                    country_codes = CountryCodeTrie,
                    extension_regexp = ExtensionMP,
                    phone_cleanup_regexp = CleanupMP
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
    Country = #country{
                 country_codes = CountryCodes,
                 iso_code = IsoCode,
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


-spec normalize_did_internal(phone_number(), [parse_option()], #state{}) -> phone_number().
normalize_did_internal(<<$+, PhoneNumber/binary>>, _Options, _State) ->
    CleanNumber = << <<Digit>> || <<Digit>> <= PhoneNumber, (Digit >= $0 andalso Digit =< $9) orelse Digit =:= $x >>,
    <<$+, CleanNumber/binary>>;
normalize_did_internal(PhoneNumber, Options, State) ->
    CountryCode = proplists:get_value(country_code, Options, State#state.default_country_code),
    CleanNumber = << <<Digit>> || <<Digit>> <= PhoneNumber, (Digit >= $0 andalso Digit =< $9) orelse Digit =:= $x >>,
    <<$+, CountryCode/binary, CleanNumber/binary>>.


-spec parse_did_internal(phone_number(), [parse_option()], #state{}) -> proplists:proplist().
parse_did_internal(FullPhoneNumber, Options, State) ->
    {PhoneNumber, Extension} = split_extension_internal(FullPhoneNumber, State),
    NormalizedNumber = normalize_did_internal(PhoneNumber, Options, State),
    {CountryCode, DomesticNumber} = split_country_code_internal(NormalizedNumber, State),
    Tail = case Extension of
               undefined -> [];
               _         -> [{extension, Extension}]
           end,
    [{country_code, CountryCode}, {phone_number, DomesticNumber} | Tail].


%% @doc Remove non-digit characters from a dialed (destination) phone number.
-spec normalize_destination_internal(phone_number(), [parse_option()]) -> phone_number().
normalize_destination_internal(PhoneNumber, _Options) when is_binary(PhoneNumber) ->
    << <<Digit>> || <<Digit>> <= PhoneNumber, Digit >= $0, Digit =< $9 >>.


-spec parse_destination_internal(phone_number(), [parse_option()], #state{}) ->
                                        {ok, proplists:proplist()} | {error, Reason :: term()}.
parse_destination_internal(FullPhoneNumber, Options, State) ->
    {PhoneNumber, Extension} = split_extension_internal(FullPhoneNumber, State),
    NormalizedNumber = normalize_destination_internal(PhoneNumber, Options),
    IsoCode = proplists:get_value(iso_code, Options, State#state.default_iso_code),
    case dict:find(IsoCode, State#state.iso_codes) of
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


-spec split_country_code_internal(phone_number(), #state{}) -> {country_code() | undefined, phone_number()}.
split_country_code_internal(<<$+, PhoneNumber/binary>>, State) ->
    split_country_code_internal(PhoneNumber, State);
split_country_code_internal(PhoneNumber, State) ->
    CleanNumber = << <<Digit>> || <<Digit>> <= PhoneNumber, Digit >= $0, Digit =< $9 >>,
    case trie:find_prefix_longest(binary_to_list(CleanNumber), State#state.country_codes) of
        {ok, CountryCodeStr, _Country} ->
            CountryCode = list_to_binary(CountryCodeStr),
            CountryCodeLen = byte_size(CountryCode),
            <<CountryCode:CountryCodeLen/binary, DomesticNumber/binary>> = CleanNumber,
            {CountryCode, DomesticNumber};
        error ->
            {undefined, PhoneNumber}
    end.


-spec split_extension_internal(phone_number(), #state{}) -> {phone_number(), extension() | undefined}.
split_extension_internal(FullPhoneNumber, State) ->
    case re:run(FullPhoneNumber, State#state.extension_regexp) of
        {match, [{PhoneLen, _} | Tail]} ->
            %% Get the position of the extension number from the last match group
            [{ExtPos, ExtLen} | _] = lists:reverse(Tail),
            SepLen = ExtPos - PhoneLen,
            <<DirtyPhoneNumber:PhoneLen/binary, _Sep:SepLen/binary, Extension:ExtLen/binary, _Rest/binary>> = FullPhoneNumber,
            %% Remove the cruft that may have been left at the end of the phone number
            case re:run(DirtyPhoneNumber, State#state.phone_cleanup_regexp) of
                {match, [{CleanLen, _} | _Tail1]} ->
                    <<CleanNumber:CleanLen/binary, _Rest1/binary>> = DirtyPhoneNumber,
                    {CleanNumber, Extension};
                nomatch ->
                    {DirtyPhoneNumber, Extension}
            end;
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
