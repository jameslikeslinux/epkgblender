-module(epkgblender_user_server).
-behavior(gen_server).
-export([start_link/0, register_user/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("epkgblender.hrl").

%%
%% API
%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register_user(Username, Password, Email) ->
    gen_server:call(?MODULE, {register_user, Username, Password, Email}).

%%
%% Callbacks
%%
init([]) -> {ok, []}.

handle_call({register_user, Username, Password, Email}, _From, State) ->
    PasswordHash = bcrypt:hashpw(Password, bcrypt:gen_salt()),
    User = #epkgblender_user{username = Username, password_hash = PasswordHash, email = Email},

    F = fun() ->
        case mnesia:read({epkgblender_user, Username}) of
            [] ->
                mnesia:write(User);
            _ ->
                mnesia:abort(user_already_exists)
        end
    end,

    case mnesia:transaction(F) of
        {aborted, user_already_exists} ->
            {reply, user_already_exists, State};
        {atomic, ok} ->
            generate_email_validation(User),
            {reply, ok, State}
    end.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%
%% Private Functions
%%
generate_email_validation(User) ->
    ValidationToken = "",   % TODO: Generate token
    mnesia:transaction(fun() -> mnesia:write(User#epkgblender_user{validation_token = ValidationToken}) end).
    %% TODO: Send email
