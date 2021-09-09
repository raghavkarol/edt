%%
%% Copyright 2020 Raghav Karol.
%%
-module(edt_http).

-export([allowed_methods/2,
         content_types_provided/2,
         init/2,
         resource_exists/2]).

-export([to_text/2,
         to_json/2]).

%% ---------------------------------------------------------
%% Callbacks
%% ---------------------------------------------------------
init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    Spec = [{<<"text/plain">>, to_text},
            {<<"application/json">>, to_json}],
    {Spec, Req, State}.

resource_exists(#{path := <<"/edt/compile", _/binary>>}=Req, State) ->
    {true, Req, State};
resource_exists(#{path := <<"/edt/trace", _/binary>>}=Req, State) ->
    {true, Req, State};
resource_exists(Req, State) ->
    {false, Req, State}.

to_text(#{path := <<"/edt/compile">>}=Req, State) ->
    Result = handle(Req),
    Body = edt_compile_result:format(Result, text),
    {Body, Req, State};
to_text(#{path := <<"/edt/trace">>}=Req, State) ->
    Body = handle(Req),
    {Body, Req, State};
to_text(#{path := _}=Req, State) ->
    {<<"NOT FOUND">>, Req, State}.

to_json(#{path := <<"/edt/compile">>}=Req, State) ->
    Result = handle(Req),
    Body = edt_compile_result:format(Result, json),
    {jiffy:encode(Body), Req, State};
to_json(#{path := <<"/edt/trace">>}=Req, State) ->
    Body = handle(Req),
    {jiffy:encode(Body), Req, State}.

%% ---------------------------------------------------------
%% Handlers
%% ---------------------------------------------------------
handle(#{path := <<"/edt/compile">>}=Req) ->
    #{path := Path} = cowboy_req:match_qs([path], Req),
    Path1 = edt_lib:to_string(Path),
    edt:compile(Path1, [strong_validation]);
handle(#{path := <<"/edt/trace">>}=Req) ->
    #{module := Module,
      function := Func} = cowboy_req:match_qs([module, function], Req),
    Opts = #{track_calls => true, max_calls => 10},
    Specs = [{edt_lib:to_atom(Module), edt_lib:to_atom(Func), #{capture => true}}],
    Result = edt_profile:trace_opts(Specs, Opts),
    edt_lib:to_binary(Result).
