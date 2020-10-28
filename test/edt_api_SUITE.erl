-module(edt_api_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap, {seconds, 5}}].

init_per_suite(Config) ->
    {ok, Home} = ct_helper:setup_test_data(Config),
    [{home, Home}|Config].

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

all() ->
    [test_compile,
     test_ct,
     test_eunit].

test_compile(Config) ->
    Home = ?config(home, Config),
    ok = edt_api:compile(Home ++ "/src/test_compile_ok.erl"),
    ok = edt_api:compile(test_compile_ok),
    ok.

test_ct(Config) ->
    Home = ?config(home, Config),
    ok = edt_api:compile(Home ++ "/test/ct_SUITE.erl"),
    ok = edt_api:test(ct_SUITE),
    ok.

test_eunit(Config) ->
    Home = ?config(home, Config),
    ok = edt_api:compile(Home ++ "/test/eunit_test1.erl"),
    ok = edt_api:test(eunit_test),
    ok.
