PROJECT_NAME := ephone

REBAR := $(shell which rebar || echo ./rebar)
ERL := erl
EPATH := -pa ./ebin -pz deps/*/ebin
TEST_EPATH := -pa ./test -pz deps/*/ebin
ERL_LIB_DIR := $(shell if [ -d /usr/lib/erlang/lib ] ; then echo /usr/lib/erlang/lib ; else echo /usr/local/lib/erlang/lib ; fi)

.PHONY: all clean compile console depclean deps doc dialyze distclean doc fast test test-console

all: compile

clean:
	$(REBAR) skip_deps=true clean

compile:
	@$(REBAR) compile

console:
	$(ERL) -sname $(PROJECT_NAME) $(EPATH)

depclean:
	$(REBAR) clean

deps:
	$(REBAR) get-deps update-deps

dialyze: compile
	@dialyzer --fullpath -Wno_undefined_callbacks src/*.erl

distclean:
	$(REBAR) clean delete-deps

doc:
	$(REBAR) skip_deps=true doc

fast:
	$(REBAR) skip_deps=true compile

test:
	@$(REBAR) ct

test-console:
	$(ERL) -sname $(PROJECT_NAME)_test $(TEST_EPATH)

