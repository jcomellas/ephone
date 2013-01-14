PROJECT_NAME := ephone

REBAR := $(shell which rebar || echo ./rebar)
ERL := erl
EPATH := -pa ./ebin -pz deps/*/ebin
TEST_EPATH := -pa .eunit -pz deps/*/ebin
ERL_LIB_DIR := $(shell if [ -d /usr/lib/erlang/lib ] ; then echo /usr/lib/erlang/lib ; else echo /usr/local/lib/erlang/lib ; fi)
PLT_APPS := $(shell ls $(ERL_LIB_DIR) | grep -v interface | sed -e 's/-[0-9.]*//')

.PHONY: all build-plt compile console deps doc clean depclean distclean dialyze release test test-console

all: compile

build-plt:
	@dialyzer --build_plt --apps $(PLT_APPS)

compile:
	@rebar compile

console:
	$(ERL) -sname $(PROJECT_NAME) $(EPATH) 

deps:
	@rebar get-deps update-deps

doc:
	@rebar skip_deps=true doc

clean:
	@rebar skip_deps=true clean

depclean:
	@rebar clean

distclean:
	@rebar clean delete-deps

dialyze: compile
	@dialyzer --fullpath -Wno_undefined_callbacks src/*.erl

fast:
	@rebar skip_deps=true compile

release: compile
	@rebar generate

fast-release: fast
	@rebar generate

test:
	@rebar skip_deps=true ct verbose=1

test-console: test
	$(ERL) -sname $(PROJECT_NAME)_test $(TEST_EPATH) -config files/sys
