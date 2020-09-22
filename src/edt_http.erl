-module(edt_http).

-export([init/2,
         content_types_provided/2,
         allowed_methods/2]).

-export([to_out/2]).

%% Callbacks
init(Req0, State) ->
    {cowboy_rest, Req0, State}.

allowed_methods(Req0, State) ->
    Methods = [<<"GET">>],
    {Methods, Req0, State}.

content_types_provided(Req0, State) ->
    {[{{ <<"text">>, <<"html">>, '*'}, to_out}], Req0, State}.

%% Output
to_out(Req0, State) ->
    #{path := Path} = cowboy_req:match_qs([path], Req0),
    Path1 = edt_lib:to_string(Path),
    Result = edt:compile(Path1, [strong_validation]),
    Body = edt_lib:report(Result),
    {Body, Req0, State}.
