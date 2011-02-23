export CC=gcc
export CFLAGS=
export LDFLAGS=

default: compile static/nitrogen

get-deps:
	./rebar get-deps

include/config.hrl:
	echo '-define(BASEDIR, "$(PWD)").' > include/config.hrl

static/nitrogen:
	ln -sf ../deps/nitrogen_core/www static/nitrogen

compile: include/config.hrl get-deps
	./rebar compile

clean:
	-rm -f static/nitrogen include/config.hrl
	./rebar delete-deps
	./rebar clean

distclean: clean
	-rm -rf deps ebin
