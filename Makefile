
MODULES= ebin/mochiweb_xpath.beam \
		 ebin/mochiweb_xpath_parser.beam \
		 ebin/mochiweb_xpath_functions.beam \
		 ebin/mochiweb_xpath_utils.beam \
		 ebin/mochiweb_html.beam \
		 ebin/test.beam \
		 ebin/mochinum.beam \
		 ebin/mochiweb_charref.beam 

all: compile

compile: $(MODULES)


ebin/%.beam : src/%.erl
	erlc -o ebin $<

clean:
	rm -f ebin/*.beam erl_crash.dump


test: compile
	erl -pa ebin -noshell -s test test  -s init stop
