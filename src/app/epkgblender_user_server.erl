-module(epkgblender_user_server).
-behavior(gen_server).
-export([start_link/0, register_user/3, validate_email/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("stdlib/include/qlc.hrl").
-include("epkgblender.hrl").

%%
%% API
%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register_user(Username, Password, Email) ->
    gen_server:call(?MODULE, {register_user, Username, Password, Email}).

validate_email(Username, ValidationToken) ->
    gen_server:call(?MODULE, {validate_email, Username, ValidationToken}).

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
    end;

handle_call({validate_email, Username, ValidationToken}, _From, State) ->
    User = get_user(Username),
    case User#epkgblender_user.validation_token of
        % User is already validated if their token is empty
        "" ->
            {reply, already_validated, State};

        % If the input token matches the one in the database
        ValidationToken ->
            F = fun() ->
                % Find any other users with the same *valid* email address
                case qlc:e(qlc:q([U || U <- mnesia:table(epkgblender_user),
                                       U#epkgblender_user.email == User#epkgblender_user.email,
                                       U#epkgblender_user.validation_token == ""])) of
                    [] ->
                        mnesia:write(User#epkgblender_user{validation_token = ""});
                    _ ->
                        mnesia:abort(email_already_registered)
                end
            end,

            case mnesia:transaction(F) of
                {aborted, email_already_registered} ->
                    {reply, email_already_registered, State};
                {atomic, ok} ->
                    {reply, ok, State} 
            end;

        _ ->
            generate_email_validation(User),
            {reply, bad_validation_token, State}
    end.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%
%% Private Functions
%%
generate_email_validation(User) ->
    ValidationToken = bin_to_hexstr(crypto:rand_bytes(16)),
    mnesia:transaction(fun() -> mnesia:write(User#epkgblender_user{validation_token = ValidationToken}) end).
    %% TODO: Send email

bin_to_hexstr(Bin) ->
    lists:flatten([io_lib:format("~2.16.0b", [X]) || X <- binary_to_list(Bin)]).

get_user(Username) ->
    {atomic, [User]} = mnesia:transaction(fun() -> mnesia:read({epkgblender_user, Username}) end),
    User.
