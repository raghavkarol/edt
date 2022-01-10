%%
%% Copyright 2020 Raghav Karol.
%%
-module(edt_http).

-export([
    allowed_methods/2,
    content_types_provided/2,
    init/2,
    resource_exists/2
]).

-export([
    to_text/2,
    to_json/2
]).

-include("edt_profile.hrl").

%% ---------------------------------------------------------
%% Callbacks
%% ---------------------------------------------------------
init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    Spec = [
        {<<"text/plain">>, to_text},
        {<<"application/json">>, to_json}
    ],
    {Spec, Req, State}.

resource_exists(#{path := <<"/edt/compile", _/binary>>} = Req, State) ->
    {true, Req, State};
resource_exists(#{path := <<"/edt/trace", _/binary>>} = Req, State) ->
    {true, Req, State};
resource_exists(Req, State) ->
    {false, Req, State}.

to_text(#{path := <<"/edt/compile">>} = Req, State) ->
    Result = handle(Req),
    Body = edt_compile_result:format(Result, text),
    {Body, Req, State};
to_text(#{path := Path} = Req, State) when
    Path == <<"/edt/trace">>;
    Path == <<"/edt/trace/call_graph">>;
    Path == <<"/edt/trace/summary">>;
    Path == <<"/edt/trace/stat_call">>
->
    Body = handle(Req),
    {Body, Req, State};
to_text(#{path := _} = Req, State) ->
    {<<"NOT FOUND">>, Req, State}.

to_json(#{path := <<"/edt/compile">>} = Req, State) ->
    Result = handle(Req),
    Body = edt_compile_result:format(Result, json),
    {jiffy:encode(Body), Req, State};
to_json(#{path := <<"/edt/trace">>} = Req, State) ->
    Body = handle(Req),
    {jiffy:encode(Body), Req, State}.

%% ---------------------------------------------------------
%% Handlers
%% ---------------------------------------------------------
handle(#{path := <<"/edt/compile">>} = Req) ->
    #{path := Path} = cowboy_req:match_qs([path], Req),
    Path1 = edt_lib:to_string(Path),
    edt:compile(Path1, [strong_validation]);
handle(#{path := <<"/edt/trace/call_graph">>}) ->
    CG1 = edt_profile_pprint:call_graph1(all),
    CG2 = io_lib:format("~s", [CG1]),
    iolist_to_binary(CG2);
handle(#{path := <<"/edt/trace/summary">>}) ->
    S1 = edt_profile_pprint:summary1(all),
    S2 = io_lib:format("~s", [S1]),
    iolist_to_binary(S2);
handle(#{path := <<"/edt/trace/stat_call">>} = Req) ->
    #{id := Id} = cowboy_req:match_qs([id], Req),
    Id1 = edt_lib:to_integer(Id),
    Result =
        case edt_profile:stat_call(Id1) of
            {error, not_found} ->
                ["call ", Id, " not found"];
            Call ->
                [
                    io_lib:format("module: ~30p", [Call#crec.module]),
                    $\n,
                    io_lib:format("func: ~30p", [Call#crec.func]),
                    $\n,
                    io_lib:format("args: ~80p", [Call#crec.args]),
                    $\n,
                    io_lib:format("Result: ~80p", [Call#crec.result])
                ]
        end,
    iolist_to_binary(Result);
handle(#{path := <<"/edt/trace">>} = Req) ->
    #{
        module := Module,
        function := Func
    } = cowboy_req:match_qs([module, function], Req),
    Opts = #{track_calls => true, max_calls => 10},
    Specs = [{edt_lib:to_atom(Module), edt_lib:to_atom(Func), '_', #{capture => true}}],
    Result = edt_profile:trace_opts(Specs, Opts),
    edt_lib:to_binary(Result).
