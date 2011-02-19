%% -*- mode: nitrogen -*-
-module (epkgblender_foo).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("basedir.hrl").

main() -> #template { file=?BASEDIR ++ "/templates/bare.html" }.

title() -> "Welcome to Foo: " ++ wf:path_info().

body() ->
    #container_12 { body=[
        #grid_8 { alpha=true, prefix=2, suffix=2, omega=true, body=inner_body() }
    ]}.

inner_body() -> 
    ?PRINT(node()),
    [
        #h1 { text="Welcome to Nitrogen" },
        #p{},
        "
        If you can see this page, then your Nitrogen server is up and
        running. Click the button below to test postbacks.
        ",
        #p{}, 	
        #button { id=button, text="Click me!", postback=click },
        #p{},
        "
        Run <b>./bin/dev help</b> to see some useful developer commands.
        "
    ].
	
event(click) ->
    ?DEBUG,
    wf:replace(button, #panel { 
        body="You clicked the button!", 
        actions=#effect { effect=highlight }
    }).
