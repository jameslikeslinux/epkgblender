%% Nitrogen elements
-include_lib("nitrogen_core/include/wf.hrl").
-record(epkgblender_table, {?ELEMENT_BASE(epkgblender_table), rows, header = [], footer = []}).
-record(epkgblender_recaptcha, {?ELEMENT_BASE(epkgblender_recaptcha)}).
-record(epkgblender_recaptcha_validator, {?VALIDATOR_BASE(epkgblender_recaptcha), privkey, on_success = fun() -> ok end}).

%% Mnesia tables
-record(epkgblender_user, {username, password_hash, name, email, validation_token = "", enabled = false, roles = [], remember_me_tokens = []}).

%% Constants
-define(REMEMBER_ME_TTL, 40320).  % four weeks
