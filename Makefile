export CC=gcc
export CFLAGS=
export LDFLAGS=

ESCRIPT = /usr/bin/i86/escript

default: compile static/nitrogen

get-deps:
	./rebar get-deps

static/nitrogen:
	cp -r deps/nitrogen_core/www static/nitrogen

compile: get-deps
	./rebar compile

rel: compile
	$(ESCRIPT) ./rebar generate force=1

clean:
	-rm -rf static/nitrogen
	./rebar delete-deps
	./rebar clean

distclean: clean
	-rm -rf deps ebin rel/epkgblender
