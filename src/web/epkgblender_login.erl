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

main() -> #template{file = ?BASEDIR "/templates/base.html"}.

title() -> "Login".

content() ->
    wf:wire(submit, username, #validate{attach_to = username_status, validators = [#is_required{text = "Required"}]}),
    wf:wire(submit, password, #validate{attach_to = password_status, validators = [#is_required{text = "Required"}]}),
    wf:wire(submit, recaptcha, #validate{attach_to = recaptcha_status, validators = [
        #is_required{text = "Required"},
        #epkgblender_recaptcha_validator{text = "Invalid CAPTCHA", privkey = ?RECAPTCHA_PRIVKEY, on_success = fun() -> wf:state(attempts, 0) end}
    ]}),
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
            #tablerow{cells = [#tablecell{}, #tablecell{body = #checkbox{id = remember_me, text = "Remember Me"}}]},
            #tablerow{cells = #tablecell{id = blank_line}},
            #tablerow{cells = [
                #tablecell{colspan = 2, body = #epkgblender_recaptcha{id = recaptcha}},
                #tablecell{body = #span{id = recaptcha_status}}
            ]},
            #tablerow{cells = [
                #tablecell{class =  "form-submit", colspan = 3, body = [#br{}, #button{id = submit, text = "Login", postback = login}]}
            ]}
        ]}
    ].

event(login) ->
    [Username, Password] = wf:mq([username, password]),

    RememberMe = case wf:q(remember_me) of
        "on" -> true;
        _ -> false
    end,

    OldRememberMeSeries = case wf:cookie(remember_me_token) of
        "" -> "";
        Token -> hd(string:tokens(Token, ":"))
    end,

    case epkgblender_user_server:authenticate(Username, Password, RememberMe, OldRememberMeSeries) of
        {error, bad_auth} ->
            wf:state(attempts, wf:state_default(attempts, 0) + 1),
            case wf:state(attempts) == 5 of
                true ->
                    wf:replace(blank_line, #tablecell{body = #br{}}),
                    epkgblender_recaptcha:create(?RECAPTCHA_PUBKEY),
                    wf:flash("Are you a robot?");
                false ->
                    wf:flash("Invalid username or password.")
            end;
        {ok, Roles, RememberMeToken} ->
            wf:user(Username),
            lists:foreach(fun(Role) -> wf:role(Role, true) end, Roles),
            case RememberMeToken of
                nil -> 
                    wf:cookie(remember_me_token, "", "/", 0);
                {Series, Value, _LastUsed} ->
                    wf:cookie(remember_me_token, Series ++ ":" ++ Value, "/", ?REMEMBER_ME_TTL)
            end,
            wf:redirect_from_login("/")
    end.
