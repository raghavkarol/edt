-module(ct_helper).

-compile([export_all, nowarn_export_all]).

setup_test_data(DataDir) ->
    {ok, Cwd} = file:get_cwd(),
    TestDir = filename:basename(Cwd),

    ok = filelib:ensure_dir("src/"),
    ok = filelib:ensure_dir("test/"),
    ok = filelib:ensure_dir("_build/default/lib/" ++ TestDir ++ "/ebin/"),
    ok = filelib:ensure_dir("_build/default/lib/" ++ TestDir ++ "/test/"),

    {ok, _} = file:copy(DataDir ++ "/test_compile_ok", "src/test_compile_ok.erl"),
    {ok, _} = file:copy(DataDir ++ "/test_compile_fail", "src/test_compile_fail.erl"),
    {ok, _} = file:copy(DataDir ++ "/ct_SUITE", "test/ct_SUITE.erl"),
    {ok, _} = file:copy(DataDir ++ "/ct_groups_SUITE", "test/ct_groups_SUITE.erl"),
    {ok, _} = file:copy(DataDir ++ "/ct_groups_empty_SUITE", "test/ct_groups_empty_SUITE.erl"),
    {ok, _} = file:copy(DataDir ++ "/eunit_test1", "test/eunit_test1.erl"),
    ok.
