%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @author Paul Oliver <puzza007@gmail.com>
%%% @copyright (C) 2012 Juan Jose Comellas, Mahesh Paolini-Subramanya,
%%%                     Paul Oliver
%%% @doc
%%% Test suite for ephone
%%% @end
%%%-------------------------------------------------------------------
-module(ephone_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").

-define(PROPTEST(A), true = proper:quickcheck(A())).

-define(CHARS, [".", ",", "(", ")", "-", "_"]).
-define(MAX_CHARS, 5).

-define(IOWA, [218237,218339,218486,218548,218632,218844,218862,218895,218936,270250,270200,270400,270402,270405,270406,270407,270431,270446,270572,270696,270834,270872,270943,270951,270960,308231,308344,308378,308526,308823,308831,308929,308256,308279,319256,319279,402590,563843,605475,605622,605715,605725,641210,641213,641235,641237,641262,641264,641297,641308,641209,641315,641388,641396,641410,641453,641509,641551,641552,641570,641594,641608,641612,641654,641665,641696,641710,641713,641715,641739,641765,641793,641795,641816,641826,641827,641865,641962,641982,641983,641985,641992,712278,712338,712353,712429,712432,712439,712451,712458,712472,712475,712541,712568,712580,712725,712726,712737,712775,712827,712858,712872,712873,712876,712889,712941,712944,712945,712948,712951,906204,906232,906239,906294,906357,906481,906874]).

-define(CANADA, [403,587,780,236,250,604,778,204,431,506,709,902,226,249,289,343,365,416,437,519,613,647,705,807,905,418,438,450,514,579,581,819,873,306,639,867]).

-define(CARIBBEAN, [242,246,264,268,297,340,345,441,473,590,592,596,649,664,670,671,684,758,764,767,784,787,809,869,876,939]).

-define(SERVICES, [111, 211, 311, 411, 511, 611, 711, 811, 911]).
-define(TF, [800, 822, 833, 844, 855, 866, 877, 888]).


suite() ->
    [{ct_hooks, [cth_surefire]}, {timetrap,{seconds,120}}].

init_per_suite(Config) ->
    start_applications(),
    start_server(Config),
    Config.


end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [
        {us, [parallel],
         [t_parse_destination_nanp_tf,
          t_parse_destination_nanp_premium,
          t_parse_destination_nanp_international,
          t_parse_destination_nanp_domestic_non_tf,
          t_parse_destination_nanp_domestic_collect,
          t_parse_destination_international,
          t_parse_destination_international_premium,
          t_parse_did_nanp,
          t_parse_destination_us_emergency,
          t_parse_destination_us_information,
          t_parse_destination_us_trs,
          t_parse_destination_us_operator,
          t_parse_destination_us_international_operator]}
    ].

all() ->
    [{group, us}].
%    [t_parse_did_nanp].


t_parse_destination_us_emergency(_) ->
    {ok, ParsedDestination} = ephone:parse_destination(<<"911">>, [{iso_code, <<"us">>}]),
    has_tag(ParsedDestination, [emergency]),
    {phone_number, <<"911">>} = lists:keyfind(phone_number, 1, ParsedDestination).

t_parse_destination_us_trs(_) ->
    {ok, ParsedDestination} = ephone:parse_destination(<<"711">>, [{iso_code, <<"us">>}]),
    has_tag(ParsedDestination, [trs]),
    {phone_number, <<"711">>} = lists:keyfind(phone_number, 1, ParsedDestination).

t_parse_destination_us_information(_) ->
    {ok, ParsedDestination} = ephone:parse_destination(<<"411">>, [{iso_code, <<"us">>}]),
    has_tag(ParsedDestination, [information]),
    {phone_number, <<"411">>} = lists:keyfind(phone_number, 1, ParsedDestination).

t_parse_destination_us_operator(_) ->
    {ok, ParsedDestination} = ephone:parse_destination(<<"0">>, [{iso_code, <<"us">>}]),
    has_tag(ParsedDestination, [operator]),
    {phone_number, <<"0">>} = lists:keyfind(phone_number, 1, ParsedDestination).

t_parse_destination_us_international_operator(_) ->
    {ok, ParsedDestination} = ephone:parse_destination(<<"00">>, [{iso_code, <<"us">>}]),
    has_tag(ParsedDestination, [international, operator]),
    {phone_number, <<"00">>} = lists:keyfind(phone_number, 1, ParsedDestination).

t_parse_did_nanp(_) ->
    ?PROPTEST(prop_parse_did_nanp).

prop_parse_did_nanp() ->
    numtests(1000, ?FORALL({Number, CountryCode, Digits, Extension}, did(nanp),
                           check_did(Number, CountryCode , Digits, Extension))).

t_parse_destination_nanp_tf(_) ->
    ?PROPTEST(prop_parse_destination_nanp_tf).

prop_parse_destination_nanp_tf() ->
    numtests(1000, ?FORALL({Number, CountryCode}, destination(nanp_tf),
                           check_destination(Number, CountryCode, [toll_free]))).

t_parse_destination_nanp_premium(_) ->
    ?PROPTEST(prop_parse_destination_nanp_premium).

prop_parse_destination_nanp_premium() ->
    numtests(1000, ?FORALL({Number, CountryCode}, destination(nanp_premium),
                           check_destination(Number, CountryCode, [premium]))).

t_parse_destination_nanp_international(_) ->
    ?PROPTEST(prop_parse_destination_nanp_international).

prop_parse_destination_nanp_international() ->
    numtests(1000, ?FORALL({Number, CountryCode}, destination(nanp_international),
                           check_destination(Number, CountryCode, [international]))).

t_parse_destination_nanp_domestic_non_tf(_) ->
    ?PROPTEST(prop_parse_destination_nanp_domestic_non_tf).

prop_parse_destination_nanp_domestic_non_tf() ->
    numtests(1000, ?FORALL({Number, CountryCode}, destination(nanp_domestic_non_tf),
                           check_destination(Number, CountryCode, [domestic]))).

t_parse_destination_nanp_domestic_collect(_) ->
    ?PROPTEST(prop_parse_destination_nanp_domestic_collect).

prop_parse_destination_nanp_domestic_collect() ->
    numtests(1000, ?FORALL({Number, CountryCode}, destination(nanp_domestic_collect),
                           check_destination(Number, CountryCode, [domestic, collect]))).

t_parse_destination_international(_) ->
    ?PROPTEST(prop_parse_destination_international).

prop_parse_destination_international() ->
    numtests(1000, ?FORALL({Number, CountryCode}, destination(international),
                           check_destination(Number, CountryCode, [international]))).

t_parse_destination_international_premium(_) ->
    ?PROPTEST(prop_parse_destination_international_premium).

prop_parse_destination_international_premium() ->
    numtests(1000, ?FORALL({Number, CountryCode}, destination(international_premium),
                           check_destination(Number, CountryCode, [international, premium]))).

%
%  INTERNAL
%

%% Initialization
start_applications() ->
    application:start(sasl),
    application:start(trie),
    application:start(kvc),
    application:start(jsx),
    application:start(ephone).

start_server(Config) ->
    setup_environment(Config).

setup_environment(_Config) ->
    random:seed(erlang:now()),
    application:set_env(ephone, default_iso_code, <<"us">>).


check_did(Did, CountryCode, PhoneNumber, Extension) ->
    try
        ParsedDid = ephone:parse_did(Did, [{country_code, CountryCode}]),
        {country_code, CountryCode} = lists:keyfind(country_code, 1, ParsedDid),
        {phone_number, PhoneNumber} = lists:keyfind(phone_number, 1, ParsedDid),
        {extension, Extension} = lists:keyfind(extension, 1, ParsedDid),

        % Normalize the DID and check it
        CLen = byte_size(CountryCode),
        PLen = byte_size(PhoneNumber),
        <<"+", CountryCode:CLen/binary, PhoneNumber:PLen/binary, 
          "x", Extension/binary>> = ephone:normalize_did(Did, [{country_code, CountryCode}]),
        true
    catch
        _:_ ->
            false
    end.

check_destination(Did, IsoCode, Tags) ->
    try
        {ok, ParsedDestination} = ephone:parse_destination(Did, [{iso_code, IsoCode}]),
        has_tag(ParsedDestination, Tags),
        {phone_number, PaDid} = lists:keyfind(phone_number, 1, ParsedDestination),
        is_validated_did(PaDid, Did, IsoCode)
    catch
        _:_ ->
            false
    end.


has_tag(ParsedDid, Tags) ->
    {billing_tags, FoundTags} = lists:keyfind(billing_tags, 1, ParsedDid),
    lists:foreach(fun(Tag) -> true = lists:member(Tag, FoundTags)
        end, Tags).


is_validated_did(PreParsedDid, Did, IsoCode) ->
    % check the returned did using parse_did too
    [CountryCode] = ephone:country_codes(IsoCode),
    ParsedDid1 = ephone:parse_did(Did, [{country_code, CountryCode}]),
    {phone_number, PDid1} = lists:keyfind(phone_number, 1, ParsedDid1),
    ParsedDid2 = ephone:parse_did(PreParsedDid, [{country_code, CountryCode}]),
    {phone_number, PDid1} = lists:keyfind(phone_number, 1, ParsedDid2),
    true.


did(Type) ->
    ?LET({Prefix, Npa, Nxx, Xxxx, Delimiter, Extension}, 
         nanp_list(Type), 
         random_did(Prefix, Npa, Nxx, Xxxx, Delimiter, Extension)).

destination(Type) ->
    ?LET({Prefix, Npa, Nxx, Xxxx, Delimiter, Extension}, 
         nanp_list(Type), 
         random_destination(Prefix, Npa, Nxx, Xxxx, Delimiter, Extension)).

nanp_list(Type) ->
    {prefix_list(Type), 
     npa_list(Type), nxx_list(Type), xxxx_list(Type), 
     delimiter_list(Type), extension_list(Type)}.

prefix_list(nanp_domestic_collect) -> "0";
prefix_list(international) -> "011";
prefix_list(international_premium) -> "011";
prefix_list(_) -> oneof(["+1", "1", ""]).

npa_list(Type) -> 
    ?LET(INpa, npa_list_integer(Type), integer_to_list(INpa)).

npa_list_integer(nanp_tf) -> oneof(?TF);
npa_list_integer(nanp_premium) -> 900;
npa_list_integer(nanp_international) -> oneof([oneof(?CANADA), oneof(?CARIBBEAN)]);
% Anything non NANP
npa_list_integer(international) -> integer(2,9);
% Lichtenstein
npa_list_integer(international_premium) -> 423;
npa_list_integer(nanp_domestic_collect) -> npa_list_integer(nanp_domestic_non_tf);
npa_list_integer(nanp_domestic_non_tf) -> 
    ?SUCHTHAT(Npa, integer(200, 999), not(lists:member(Npa, ?CANADA)
                                          orelse lists:member(Npa, ?TF)
                                          orelse lists:member(Npa, ?CARIBBEAN)
                                          orelse lists:member(Npa, ?SERVICES)
                                          orelse Npa =:= 900));
npa_list_integer(nanp) -> oneof([npa_list_integer(nanp_tf),
                                npa_list_integer(nanp_premium),
                                npa_list_integer(nanp_international),
                                npa_list_integer(nanp_domestic_non_tf)]).

nxx_list(international) -> 
    oneof([?LET(X, integer(0,999999999), integer_to_list(X)),
           ?LET(X, integer(0,99999999), "0" ++ integer_to_list(X)),
           ?LET(X, integer(0,9999999), "00" ++ integer_to_list(X)),
           ?LET(X, integer(0,999999), "000" ++ integer_to_list(X)),
           ?LET(X, integer(0,99999), "0000" ++ integer_to_list(X)),
           ?LET(X, integer(0,9999), "00000" ++ integer_to_list(X)),
           ?LET(X, integer(0,999), "000000" ++ integer_to_list(X)),
           ?LET(X, integer(0,99), "0000000" ++ integer_to_list(X)),
           ?LET(X, integer(0,9), "00000000" ++ integer_to_list(X))]);
nxx_list(_) -> 
    ?LET(X, integer(200,999), integer_to_list(X)).

xxxx_list(_) -> 
    [integer($0, $9), integer($0, $9), integer($0, $9), integer($0, $9)].

delimiter_list(_) -> 
    ?LET({Str, Dot}, delimiter_list_strings(), Str ++ Dot).

delimiter_list_strings() -> 
    {oneof(["x", "ex", "xt", "ext"]), oneof([".", ""])}.

extension_list(_) ->
    oneof([?LET(X, integer(0,999999), integer_to_list(X)),
           ?LET(X, integer(0,99999), "0" ++ integer_to_list(X)),
           ?LET(X, integer(0,9999), "00" ++ integer_to_list(X)),
           ?LET(X, integer(0,999), "000" ++ integer_to_list(X)),
           ?LET(X, integer(0,99), "0000" ++ integer_to_list(X)),
           ?LET(X, integer(0,9), "00000" ++ integer_to_list(X))]).



random_destination(Prefix, Npa, Nxx, Xxxx, Delimiter, Extension) ->
    Number = sanitize(Npa ++ Nxx ++ Xxxx),
    IsoCode = "us",
    FullNumber = sanitize(Prefix ++ random_chars(Number) ++  Delimiter ++ Extension),
    {list_to_binary(FullNumber), list_to_binary(IsoCode)}.

random_did(Prefix, Npa, Nxx, Xxxx, Delimiter, Extension) ->
    Number = sanitize(Npa ++ Nxx ++ Xxxx),
    CountryCode = "1",
    FullNumber = sanitize(Prefix ++ random_chars(Number) ++  Delimiter ++ Extension),
    {list_to_binary(FullNumber), list_to_binary(CountryCode), list_to_binary(Number), list_to_binary(Extension)}.

%% Utility functions used to randomize strings

sanitize(X) -> lists:flatten(X).

random_chars([H | T]) -> [maybe_char([H]) | random_chars(T)];
random_chars([]) -> [].

maybe(X, Y) ->
    case random:uniform(2) of
        1 -> X;
        2 -> Y
    end.

maybe_char() ->
    Chars = chars(),
    lists:map(fun(_) -> maybe(Chars, "") end, lists:seq(1, ?MAX_CHARS)).
maybe_char(String) ->
    lists:flatten(lists:map(fun(X) ->
                maybe_char() ++ [X] ++ maybe_char()
        end, String)).

chars() ->
    lists:flatten(lists:map(fun(_) -> maybe(random_item(?CHARS), "") end, lists:seq(1, ?MAX_CHARS))).

random_item(List) ->
    lists:nth(random:uniform(erlang:length(List)), List).

