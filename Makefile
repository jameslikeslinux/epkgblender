PATH = /usr/bin/i86:/usr/bin:/bin
REL = test

default: compile

get-deps:
	./rebar get-deps

static/nitrogen:
	cp -r deps/nitrogen_core/www static/nitrogen

compile: get-deps static/nitrogen
	./rebar compile

rel: compile
	cd rel/$(REL); ../../rebar generate

clean:
	-rm -rf static/nitrogen
	./rebar clean

distclean: clean
	./rebar delete-deps
	-rm -rf deps ebin rel/prod/epkgblender* rel/test/epkgblender*
