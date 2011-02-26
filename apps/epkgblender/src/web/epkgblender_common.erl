%%%
%%% epkgblender_common.erl
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

-module(epkgblender_common).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

left_toolbar_links() -> [
    #link{text = "Dashboard", url = "#"},
    #link{text = "Packages", url = "#"},
    #link{text = "Builds", url = "#"},
    #link{text = "Upload", url = "/upload", show_if = wf:role(user)}
].

right_toolbar_links() ->
    case wf:user() of
        undefined -> [
            #link{text = "Login", url = "/login"},
            #link{text = "Register", url = "/register"}
        ];

        _User -> [
            #link{text = "Logout", url = "/logout"},
            #link{text = "Account Settings", url = "/accountsettings"},
            #link{text = "Admin", url = "/admin", show_if = wf:role(admin)}
        ]
    end.
