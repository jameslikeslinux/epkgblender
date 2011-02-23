%%%
%%% epkgblender_user_server.erl
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

-module(epkgblender_user_server).
-behavior(gen_server).
-export([start_link/0, register_user/4, validate_email/2, authenticate/2, user_exists/1, email_registered/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("stdlib/include/qlc.hrl").
-include("epkgblender.hrl").

%%
%% API
%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register_user(Username, Password, Name, Email) ->
    gen_server:call(?MODULE, {register_user, Username, Password, Name, Email}).

validate_email(Username, ValidationToken) ->
    gen_server:call(?MODULE, {validate_email, Username, ValidationToken}).

authenticate(Username, Password) ->
    gen_server:call(?MODULE, {authenticate, Username, Password}).

user_exists(Username) ->
    gen_server:call(?MODULE, {user_exists, Username}).

email_registered(Email) ->
    gen_server:call(?MODULE, {email_registered, Email}).


%%
%% Callbacks
%%
init([]) ->
    mnesia:create_table(epkgblender_user, [{disc_copies, [node()]}, {attributes, record_info(fields, epkgblender_user)}]),
    {ok, []}.

handle_call({register_user, Username, Password, Name, Email}, _From, State) ->
    PasswordHash = bcrypt:hashpw(Password, bcrypt:gen_salt()),
    User = #epkgblender_user{username = Username, password_hash = PasswordHash, name = Name, email = Email},

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
    {ok, User} = get_user(Username),
    case User#epkgblender_user.validation_token of
        % User is already validated if their token is empty
        "" ->
            {reply, already_validated, State};

        % If the input token matches the one in the database
        ValidationToken ->
            F = fun() ->
                case get_user_by_email(User#epkgblender_user.email) of
                    {error, no_such_user} ->
                        mnesia:write(User#epkgblender_user{validation_token = ""});
                    {ok, _User} ->
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
    end;

handle_call({authenticate, Username, Password}, _From, State) ->
    case get_user(Username) of
        {ok, User} ->
            PasswordHash = User#epkgblender_user.password_hash,
            case PasswordHash == bcrypt:hashpw(Password, PasswordHash) of
                true ->
                    {reply, {ok, User#epkgblender_user.roles}, State};
                false ->
                    {reply, {error, bad_auth}, State}
            end;
        {error, no_such_user} ->
            {reply, {error, bad_auth}, State}
    end;

handle_call({user_exists, Username}, _From, State) ->
    case get_user(Username) of
        {ok, _User} ->
            {reply, true, State};
        {error, no_such_user} ->
            {reply, false, State}
    end;

handle_call({email_registered, Email}, _From, State) ->
    case get_user_by_email(Email) of
        {ok, _User} ->
            {reply, true, State};
        {error, no_such_user} ->
            {reply, false, State}
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
    case mnesia:transaction(fun() -> mnesia:read({epkgblender_user, Username}) end) of
        {atomic, []} -> {error, no_such_user};
        {atomic, User} -> {ok, User}
    end.

get_user_by_email(Email) ->
    F = fun() ->
        qlc:e(qlc:q([U || U <- mnesia:table(epkgblender_user),
                          U#epkgblender_user.email == Email,
                          U#epkgblender_user.validation_token == ""]))
    end,

    case mnesia:transaction(F) of
        {atomic, []} -> {error, no_such_user};
        {atomic, [User]} -> {ok, User}
    end.
