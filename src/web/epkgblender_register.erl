%%%
%%% epkgblender_register.erl
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

-module(epkgblender_register).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("config.hrl").
-include("epkgblender.hrl").

main() -> #template{file = ?BASEDIR ++ "/templates/base.html"}.

title() -> "Create Account".

content() ->
    wf:wire(submit, username, #validate{attach_to = username_status, validators = [
        #is_required{text = "Required"},
        #custom{text = "Invalid username", function = fun matches/2, tag = "^[a-z][a-z0-9_-]{1,6}[a-z0-9]$"},
        #custom{text = "Username already registered", function = fun user_not_exists/2}
    ]}),
    wf:wire(submit, password, #validate{attach_to = password_status, validators = [
        #is_required{text = "Required"},
        #min_length{text = "Must have at least 8 characters", length = 8},
        #custom{text = "Must have at least one lower-case character", function = fun matches/2, tag = "[a-z]"},
        #custom{text = "Must have at least one upper-case character", function = fun matches/2, tag = "[A-Z]"},
        #custom{text = "Must have at least one digit", function = fun matches/2, tag = "[0-9]"}
    ]}),
    wf:wire(submit, confirm_password, #validate{attach_to = confirm_password_status, validators = [
        #is_required{text = "Required"},
        #confirm_password{text = "Passwords must match", password = password}
    ]}),
    wf:wire(submit, name, #validate{attach_to = name_status, validators = [#is_required{text = "Required"}]}),
    wf:wire(submit, email, #validate{attach_to = email_status, validators = [
        #is_required{text = "Required"},
        #is_email{text = "Not a valid e-mail address"},
        #custom{text = "E-mail address already registered", function = fun email_not_registered/2}
    ]}),
    wf:wire(submit, recaptcha_response_field, #validate{attach_to = recaptcha_status, validators = [
        #is_required{text = "Required"},
        #epkgblender_recaptcha_validator{text = "Invalid CAPTCHA", privkey = ?RECAPTCHA_PRIVKEY}
    ]}),
    [
        #h1{text = "Create Account"},
        #epkgblender_table{rows = [
            #tablerow{cells = [
                #tablecell{colspan = 3, body = #flash{}}
            ]},
            #tablerow{cells = [
                #tablecell{class = "form-labels", body = ["Username:", #br{}, #span{class = "hint", text = "(Unix-style username)"}]},
                #tablecell{body = #textbox{id = username, next = password}},
                #tablecell{body = #span{id = username_status}}
            ]},
            #tablerow{cells = [
                #tablecell{class = "form-labels", body = ["Password:", #br{}, #span{class = "hint", text = "(≥ 8 characters, 1 a-z, 1 A-Z, 1 0-9)"}]},
                #tablecell{body = #password{id = password, next = confirm_password}},
                #tablecell{body = #span{id = password_status}}
            ]},
            #tablerow{cells = [
                #tablecell{class = "form-labels", text = "Confirm Password:"},
                #tablecell{body = #password{id = confirm_password, next = name}},
                #tablecell{body = #span{id = confirm_password_status}}
            ]},
            #tablerow{cells = [
                #tablecell{class = "form-labels", body = ["Full Name:", #br{}, #span{class = "hint", text = "(How it will be displayed on this site)"}]},
                #tablecell{body = #textbox{id = name, next = email}},
                #tablecell{body = #span{id = name_status}}
            ]},
            #tablerow{cells = [
                #tablecell{class = "form-labels", text = "E-mail Address:"},
                #tablecell{body = #textbox{id = email, next = submit}},
                #tablecell{body = #span{id = email_status}}
            ]},
            #tablerow{cells = #tablecell{body = #br{}}},
            #tablerow{cells = [
                #tablecell{colspan = 2, body = #epkgblender_recaptcha{id = recaptcha, pubkey = ?RECAPTCHA_PUBKEY}},
                #tablecell{body = #span{id = recaptcha_status}}
            ]},
            #tablerow{cells = [
                #tablecell{class =  "form-submit", colspan = 3, body = [#br{}, #button{id = submit, text = "Register", postback = register}]}
            ]}
        ]}
    ].

event(register) ->
    wf:flash("Registered!").

matches(Regex, String) ->
    case re:run(String, Regex) of
        {match, _} -> true;
        nomatch -> false
    end.

user_not_exists(_Tag, Username) ->
    not epkgblender_user_server:user_exists(Username).

email_not_registered(_Tag, Email) ->
    not epkgblender_user_server:email_registered(Email).
