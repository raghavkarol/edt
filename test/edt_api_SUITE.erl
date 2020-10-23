-module(edt_api_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap, {seconds, 5}}].

init_per_suite(Config) ->
    DataDir = ?config(data_dir, Config),
    ct_helper:setup_test_data(DataDir),
    Config.

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

test_compile(_Config) ->
    ok = edt_api:compile("src/test_compile_ok.erl"),
    ok = edt_api:compile(test_compile_ok),
    ok.

test_ct(_Config) ->
    ok = edt_api:compile("test/ct_SUITE.erl"),
    ok = edt_api:test(ct_SUITE),
    ok.

test_eunit(_Config) ->
    ok = edt_api:compile("test/eunit_test.erl"),
    ok = edt_api:test(eunit_test),
    ok.
