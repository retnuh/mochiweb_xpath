REBAR=./rebar
DIALYZER=dialyzer

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

dialyze: .dialyzer_plt ~/.dialyzer_plt
	$(DIALYZER) -I deps --plts .dialyzer_plt ~/.dialyzer_plt -- ebin

~/.dialyzer_plt:
	$(DIALYZER) --build_plt --apps erts kernel stdlib crypto mnesia sasl eunit xmerl

.dialyzer_plt:
	$(DIALYZER) --build_plt --output_plt .dialyzer_plt -r deps/*/ebin

.PHONY: test deps
