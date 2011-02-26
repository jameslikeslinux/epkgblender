PATH = /usr/bin/i86:/usr/bin

default: compile

get-deps:
	./rebar get-deps

static/nitrogen:
	cp -r deps/nitrogen_core/www static/nitrogen

compile: get-deps static/nitrogen
	./rebar compile

rel: compile
	./rebar generate

clean:
	-rm -rf static/nitrogen
	./rebar clean

distclean: clean
	./rebar delete-deps
	-rm -rf deps ebin rel/epkgblender*
