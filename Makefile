default: compile nitrogen-www

get-deps:
	./rebar get-deps

nitrogen-www: get-deps
	ln -sf ../deps/nitrogen_core/www static/nitrogen

compile: get-deps
	./rebar compile

clean:
	-rm -f static/nitrogen
	./rebar delete-deps
	./rebar clean

distclean: clean
	-rm -rf deps ebin
