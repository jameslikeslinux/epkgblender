%%%
%%% epkgblender_recaptcha.erl
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

-module(epkgblender_recaptcha).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("epkgblender.hrl").

reflect() ->
    record_info(fields, epkgblender_recaptcha).

render_element(Record) ->
    RecaptchaDivId = wf:temp_id(),
    RecaptchaScript = wf:f("
        <script type='text/javascript' src='http://www.google.com/recaptcha/api/js/recaptcha_ajax.js'></script>
        <script type='text/javascript'>
            var recaptcha_div_id = '~s';
            var recaptcha_response_field_class = '~s';
        </script>
    ", [RecaptchaDivId, tl(Record#epkgblender_recaptcha.id)]),
    [RecaptchaScript, #panel{
        id = RecaptchaDivId,
        % Include an invisible response field in the DOM so validators
        % don't freak out when the recaptcha hasn't been created yet.
        body = #textbox{style = "display: none;", id = Record#epkgblender_recaptcha.id, text = "dummy"}
    }].

render_action(Record) ->
    #custom{
        trigger = Record#epkgblender_recaptcha_validator.trigger,
        target = Record#epkgblender_recaptcha_validator.target,
        text = Record#epkgblender_recaptcha_validator.text,
        attach_to = Record#epkgblender_recaptcha_validator.attach_to,
        function = fun validate/2,
        tag = Record
    }.

validate(Record, Response) ->
    case wf:state_default(is_human, true) of
        true ->
            true;
        false ->
            PostData = wf:f("privatekey=~s&remoteip=~s&challenge=~s&response=~s", [
                wf:url_encode(Record#epkgblender_recaptcha_validator.privkey),
                wf:url_encode(remote_ip()),
                wf:url_encode(wf:q(recaptcha_challenge_field)),
                wf:url_encode(Response)
            ]),
            case httpc:request(post, {"http://www.google.com/recaptcha/api/verify", [], "application/x-www-form-urlencoded", PostData}, [{timeout, 10000}], [{full_result, false}]) of
                {ok, {200, "true" ++ _Rest}} ->
                    wf:state(is_human, true),
                    (Record#epkgblender_recaptcha_validator.on_success)(),
                    true;
                _ ->
                    wf:wire(#script{script = "Recaptcha.reload();"}),
                    false
            end
    end.

create(Pubkey) ->
    wf:state(is_human, false),
    wf:wire(#script{script = wf:f("
        Recaptcha.create('~s', obj(recaptcha_div_id), {
            callback: function() {
                // Set wfids on the input fields so they will be submitted to Nitrogen.
                objs('#recaptcha_challenge_field').addClass('wfid_recaptcha_challenge_field');
                objs('#recaptcha_response_field').addClass(recaptcha_response_field_class);
            }
        });

        // Setting focus_response_field has two desired effects:
        //   1. Disables focusing on response field after a validation/recaptcha reload.
        //   2. Sets a wfid on the new challenge field so it can be submitted to Nitrogen.
        Recaptcha.focus_response_field = function() {
            objs('#recaptcha_challenge_field').addClass('wfid_recaptcha_challenge_field');
        };
    ", [wf:js_escape(Pubkey)])}).

remote_ip() ->
    {ok, {Address, _Port}} = inet:peername(wf:socket()),
    inet_parse:ntoa(Address).
