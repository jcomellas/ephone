APPLICATION := ephone

REBAR=$(shell which rebar || echo ./rebar)
ERL := erl
EPATH := -pa ebin -pz deps/*/ebin
TEST_EPATH := -pa .eunit -pz deps/*/ebin

DIALYZER=dialyzer
DIALYZER_OPTS=-Wno_return -Wrace_conditions -Wunderspecs -Wno_undefined_callbacks --fullpath

.PHONY: all clean compile console deps dialyze doc test test-console

all: compile

clean:
	@$(REBAR) clean

compile:
	@$(REBAR) compile

console:
	$(ERL) -sname $(APPLICATION) $(EPATH)

deps:
	@$(REBAR) get-deps && $(REBAR) update-deps

dialyze: compile
	@$(DIALYZER) $(DIALYZER_OPTS) -r ./

doc:
	@$(REBAR) doc

test:
	@$(REBAR) ct

test-console:
	$(ERL) -sname $(APPLICATION)_test $(TEST_EPATH)

