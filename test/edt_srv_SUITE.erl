-module(edt_srv_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("edt_srv.hrl").

suite() ->
    [{timetrap, {seconds, 5}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    application:set_env(edt, mode, "src"),
    init_per_testcase1(Config).

init_per_testcase1(Config) ->
    application:set_env(edt, ignore_regex, <<"ignore">>),
    application:set_env(edt, auto_process, "false"),
    Dir = ?config(priv_dir, Config),
    edt_srv:start(Dir),
    Config.

end_per_testcase(_TestCase, _Config) ->
    edt_srv:stop(),
    ok.

all() ->
    [test_ignore_regex,
     test_src_changes].

test_src_changes(Config) ->
    PrivDir = ?config(priv_dir, Config),
    [] = edt_srv:changes(),

    Path1 = filename:join(PrivDir, "testing_1.erl"),
    create_file(Path1),
    edt_lib:retry(fun() -> edt_srv:changes() end,
          [#change{action=compile, path=Path1, count=1}]),

    create_file(Path1),
    edt_lib:retry(fun() -> edt_srv:changes() end,
          [#change{action=compile, path=Path1, count=2}]),

    Path2 = filename:join(PrivDir, "testing_2.erl"),
    create_file(Path2),
    edt_lib:retry(fun() -> edt_srv:changes() end,
          [#change{action=compile, path=Path1, count=2},
           #change{action=compile, path=Path2, count=1}]),

    delete_file(Path2),
    edt_lib:retry(fun() -> edt_srv:changes() end,
          [#change{action=compile, path=Path1, count=2}]),

    Path3 = filename:join(PrivDir, "testing_1.beam"),
    create_file(Path3),
    edt_lib:retry(fun() -> edt_srv:changes() end,
          [#change{action=compile, path=Path1, count=2}]),
    ok.

test_ignore_regex(Config) ->
    PrivDir = ?config(priv_dir, Config),

    <<"ignore">> = edt:ignore_regex(),
    Path1 = filename:join(PrivDir, "ignore/testing_1.erl"),
    ok = filelib:ensure_dir(Path1),

    os:cmd("echo \"-module(testing_1).\" >" ++ Path1),
    [] = edt_srv:changes(),
    ok.

%% @doc using os:cmd because file:write_file doesn't trigger notifications
create_file(Path) ->
    filelib:ensure_dir(Path),
    os:cmd("echo \"-module(testing_1).\" >" ++ Path),
    os:cmd("echo \"-compile([export_all, nowarn_export_all]).\" >>" ++ Path),
    ok.

delete_file(Path) ->
    os:cmd("rm " ++ Path),
    ok.
