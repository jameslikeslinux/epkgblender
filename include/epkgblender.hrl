%% Nitrogen elements
-include_lib("nitrogen_core/include/wf.hrl").
-record(epkgblender_table, {?ELEMENT_BASE(epkgblender_table), rows, header = [], footer = []}).
-record(epkgblender_recaptcha, {?ELEMENT_BASE(epkgblender_recaptcha)}).
-record(epkgblender_recaptcha_validator, {?VALIDATOR_BASE(epkgblender_recaptcha), privkey, on_success = fun() -> ok end}).

%% Mnesia tables
-record(epkgblender_user, {username, password_hash, name, email, validation_token = "", roles = [], remember_me_tokens = []}).

%% Constants
-define(REMEMBER_ME_TTL, 40320).  % four weeks
-define(RECAPTCHA_PUBKEY, "6LfUBMISAAAAAJ3cFPBx7vYD6ZMZfuk9rXJYidov").
-define(RECAPTCHA_PRIVKEY, "6LfUBMISAAAAADNOwdgxoW1wVOFoSj2hBoXKTRel").
