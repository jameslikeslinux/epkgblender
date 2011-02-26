PATH = /usr/bin/i86:/usr/bin

default: compile

get-deps:
	./rebar get-deps

apps/epkgblender/priv/static/nitrogen:
	cp -r deps/nitrogen_core/www apps/epkgblender/priv/static/nitrogen

compile: get-deps apps/epkgblender/priv/static/nitrogen
	./rebar compile

rel: compile
	./rebar generate force=1

clean:
	-rm -rf apps/epkgblender/priv/static/nitrogen
	./rebar clean

distclean: clean
	./rebar delete-deps
	-rm -rf deps apps/epkgblender/ebin rel/epkgblender*
