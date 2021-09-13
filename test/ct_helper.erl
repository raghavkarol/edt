-module(ct_helper).

-include_lib("common_test/include/ct.hrl").

-compile([export_all, nowarn_export_all]).

setup_test_data(Config) ->
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),
    TestDir = filename:basename(PrivDir),

    Src = fun(P) -> DataDir ++ P end,
    Dst = fun(P) -> PrivDir ++ P end,

    ok = filelib:ensure_dir(Dst("/src/")),
    ok = filelib:ensure_dir(Dst("/test/")),
    ok = filelib:ensure_dir("test/"),
    ok = filelib:ensure_dir("_build/default/lib/" ++ TestDir ++ "/ebin/"),
    ok = filelib:ensure_dir("_build/default/lib/" ++ TestDir ++ "/test/"),

    {ok, _} = file:copy(
        Src("/test_compile_ok"),
        Dst("/src/test_compile_ok.erl")
    ),
    {ok, _} = file:copy(
        Src("/test_compile_fail"),
        Dst("/src/test_compile_fail.erl")
    ),
    {ok, _} = file:copy(
        Src("/ct_SUITE"),
        Dst("/test/ct_SUITE.erl")
    ),
    {ok, _} = file:copy(
        Src("/ct_groups_SUITE"),
        Dst("/test/ct_groups_SUITE.erl")
    ),
    {ok, _} = file:copy(
        Src("/ct_groups_empty_SUITE"),
        Dst("/test/ct_groups_empty_SUITE.erl")
    ),
    {ok, _} = file:copy(
        Src("/eunit_test1"),
        Dst("/test/eunit_test1.erl")
    ),
    {ok, PrivDir}.
