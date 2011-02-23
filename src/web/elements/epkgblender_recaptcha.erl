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

render_element(R) ->
    RecaptchaScript = "
        <script type='text/javascript' src='http://api.recaptcha.net/challenge?k=" ++ R#epkgblender_recaptcha.pubkey ++ "'></script>
        <script>
            objs('#recaptcha_challenge_field').addClass('wfid_recaptcha_challenge_field');
            objs('#recaptcha_response_field').addClass('wfid_recaptcha_response_field');
        </script>",
    #panel{class = ["recaptcha ", R#epkgblender_recaptcha.class], body = RecaptchaScript}.
