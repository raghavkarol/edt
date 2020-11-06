-module(edt_app_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").

%% ---------------------------------------------------------
%% Common test callbacks
%% ---------------------------------------------------------
suite() ->
    [{timetrap, {seconds, 5}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(test_valid_parse_profile, Config) ->
    meck:new(edt_lib, [passthrough]),
    meck:expect(edt_lib, parse_rebar3_profile, fun(_) -> edt:rebar3_profile() end),
    [{apps, start_apps()}|Config];
init_per_testcase(_TestCase, Config) ->
    application:set_env(edt, warn_rebar3_profile, false),
    [{apps, start_apps()}|Config].

end_per_testcase(_TestCase, Config) ->
    application:set_env(edt, warn_rebar3_profile, true),
    Apps = ?config(apps, Config),
    [ application:stop(A) || A <- Apps ],
    catch meck:unload(edt_lib),
    ok.

all() ->
    [test_http,
     test_invalid_parse_profile,
     test_valid_parse_profile].
%% ---------------------------------------------------------
%% Headers
%% ---------------------------------------------------------
start_apps() ->
    application:set_env(edt, edt_out_initialized, false),
    {ok, Apps} = application:ensure_all_started(edt),
    Apps.

%% ---------------------------------------------------------
%%  Test Cases
%% ---------------------------------------------------------
test_invalid_parse_profile(_Config) ->
    Children = supervisor:which_children(edt_sup),
    Ids = lists:sort([Id || {Id, _, _, _} <- Children]),
    [edt_out, edt_post_action, edt_srv] = Ids,
    ok.

test_valid_parse_profile(_Config) ->
    Children = supervisor:which_children(edt_sup),
    Ids = lists:sort([Id || {Id, _, _, _} <- Children]),
    [edt_out, edt_post_action, edt_srv, edt_trace] = Ids,
    ok.

test_http(_Config) ->
    Result1 = httpc:request("http://localhost:65000/flycheck"),
    {ok, {{"HTTP/1.1", 400, "Bad Request"}, _Headers1, _Body1}} = Result1,

    Result2 = httpc:request("http://localhost:65000/flycheck?path=src/test_compile.erl"),
    {ok, {{"HTTP/1.1", 200, "OK"}, _Headers2, Body2}} = Result2,
    "ERROR src/test_compile.erl \nsrc/test_compile.erl:none: Error: no such file or directory\n\n" = Body2,
    ok.
