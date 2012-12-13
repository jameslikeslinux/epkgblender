PATH = /usr/bin/i86:/usr/bin:/bin
REL = test

default: rel

get-deps:
	./rebar get-deps

static/nitrogen:
	cp -r deps/nitrogen_core/www static/nitrogen

compile: get-deps static/nitrogen
	./rebar compile

stop:
	-./rel/$(REL)/epkgblender/bin/epkgblender stop

start:
	-./rel/$(REL)/epkgblender/bin/epkgblender start

console:
	-./rel/$(REL)/epkgblender/bin/epkgblender console

attach:
	-./rel/$(REL)/epkgblender/bin/epkgblender attach

rel: compile stop
	-rm -rf rel/$(REL)/epkgblender
	cd rel/$(REL); ../../rebar generate

clean:
	-rm -rf static/nitrogen
	./rebar clean

distclean: clean
	./rebar delete-deps
	-rm -rf deps ebin rel/prod/epkgblender* rel/test/epkgblender*
