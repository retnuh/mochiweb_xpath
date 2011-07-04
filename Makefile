REBAR=./rebar

all:  deps compile

deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

edoc:
	@$(REBAR) doc

test:  all
	@$(REBAR) skip_deps=true eunit

clean:
	@$(REBAR) clean

.PHONY: test deps
