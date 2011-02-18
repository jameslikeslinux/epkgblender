-module(epkgblender_rewrite).
-export([arg_rewrite/1]).

-include_lib("yaws/include/yaws_api.hrl").

arg_rewrite(Arg) ->
    Req = Arg#arg.req,
    {abs_path, Path} = Req#http_request.path,
    try yaws_api:url_decode_q_split(Path) of
        {DecPath, _Query} ->
            case DecPath == "/" orelse not filelib:is_file(Arg#arg.docroot ++ DecPath) of
                true ->
                    Arg#arg{req = Req#http_request{path = {abs_path, "/epkgblender" ++ Path}}};
                false ->
                    Arg
            end
    catch
        'EXIT':_ ->
            Arg
    end.
