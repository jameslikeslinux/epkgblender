%%%
%%% epkgblender_login.erl
%%% Copyright (C) 2011 James Lee
%%% 
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%

-module(epkgblender_login).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("config.hrl").
-include("epkgblender.hrl").

main() -> #template{file = ?BASEDIR ++ "/templates/base.html"}.

title() -> "Login".

content() ->
    wf:wire(submit, username, #validate{attach_to = username_status, validators = [#is_required{text = "Required"}]}),
    wf:wire(submit, password, #validate{attach_to = password_status, validators = [#is_required{text = "Required"}]}),
    [
        #h1{text = "Login"},
        #epkgblender_table{rows = [
            #tablerow{cells = [
                #tablecell{colspan = 3, body = #flash{}}
            ]},
            #tablerow{cells = [
                #tablecell{class = "form-labels", text = "Username:"},
                #tablecell{body = #textbox{id = username, next = password}},
                #tablecell{body = #span{id = username_status}}
            ]},
            #tablerow{cells = [
                #tablecell{class = "form-labels", text = "Password:"},
                #tablecell{body = #password{id = password, next = submit}},
                #tablecell{body = #span{id = password_status}}
            ]},
            #tablerow{cells = #tablecell{body = #br{}}},
            #tablerow{id = foo, style="display: none", cells = [
                #tablecell{colspan = 2, body = #epkgblender_recaptcha{id = recaptcha, pubkey = ?RECAPTCHA_PUBKEY}},
                #tablecell{body = #span{id = recaptcha_status}}
            ]},
            #tablerow{cells = [
                #tablecell{class =  "form-submit", colspan = 3, body = [#br{}, #button{id = submit, text = "Login", postback = login}]}
            ]}
        ]}
    ].

event(login) ->
    [Username, Password] = wf:mq([username, password]),
    case epkgblender_user_server:authenticate(Username, Password) of
        {error, bad_auth} ->
            wf:state(attempts, wf:state_default(attempts, 0) + 1),
            case wf:state(attempts) == 2 of
                true ->
                    wf:wire(foo, #show{effect = slide, speed = 500}),
                    wf:flash("Are you a robot?");
                false ->
                    wf:flash("Invalid username or password.")
            end;
        {ok, Roles} ->
            wf:user(Username),
            lists:foreach(fun(Role) -> wf:role(Role, true) end, Roles),
            wf:redirect_from_login("/")
    end.
