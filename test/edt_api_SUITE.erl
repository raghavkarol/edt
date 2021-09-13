-module(edt_api_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap, {seconds, 5}}].

init_per_suite(Config) ->
    {ok, Home} = ct_helper:setup_test_data(Config),
    [{home, Home} | Config].

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

all() ->
    [
        test_compile_ok,
        test_compile_fail,
        test_ct,
        test_eunit_ok,
        test_eunit_error
    ].

test_compile_ok(Config) ->
    Home = ?config(home, Config),
    ok = edt_api:compile(Home ++ "/src/test_compile_ok.erl"),
    ok = edt_api:compile(test_compile_ok),
    ok.

test_compile_fail(Config) ->
    Home = ?config(home, Config),
    {error, _} = edt_api:compile(Home ++ "/src/test_compile_faile.erl").

test_ct(Config) ->
    Home = ?config(home, Config),
    ok = edt_api:compile(Home ++ "/test/ct_SUITE.erl"),
    edt_api:test(ct_SUITE).

test_eunit_ok(Config) ->
    Home = ?config(home, Config),
    ok = edt_api:compile(Home ++ "/test/eunit_test1.erl"),
    ok = edt_api:test(eunit_test1),
    ok = edt_api:test(eunit_test1, test_one).

test_eunit_error(Config) ->
    Home = ?config(home, Config),
    ok = edt_api:compile(Home ++ "/test/eunit_test1.erl"),
    {error, _} = edt_api:test(eunit_test).
