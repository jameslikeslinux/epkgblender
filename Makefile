default: compile static/nitrogen

get-deps:
	./rebar get-deps

include/basedir.hrl:
	echo '-define(BASEDIR, "$(PWD)").' > include/basedir.hrl

static/nitrogen:
	ln -sf ../deps/nitrogen_core/www static/nitrogen

compile: include/basedir.hrl get-deps
	./rebar compile

clean:
	-rm -f static/nitrogen include/basedir.hrl
	./rebar delete-deps
	./rebar clean

distclean: clean
	-rm -rf deps ebin
