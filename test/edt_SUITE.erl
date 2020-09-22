-module(edt_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("edt_srv.hrl").

suite() ->
    [{timetrap, {seconds, 5}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    os:unsetenv("EDT_HOME"),
    application:unset_env(edt, home),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

all() ->
    [test_file_type,
     test_get_env,
     test_home,
     test_includes,
     test_module_name,
     test_outdir,
     test_parse_path,
     test_relative_path].

test_home(_Config) ->
    {ok, Pwd} = file:get_cwd(),
    Pwd = edt:home(),

    os:putenv("EDT_HOME", "testing1"),
    "testing1" = edt:home(),

    application:set_env(edt, home, "testing2"),
    "testing2" = edt:home(),

    ok.

test_includes(Config) ->
    PrivDir = ?config(priv_dir, Config),
    Profile = edt:rebar3_profile(),
    application:set_env(edt, home, PrivDir),

    App = [PrivDir, "_build", Profile, "lib", "app1"],
    CApp = [PrivDir, "_checkouts", "app2"],

    filelib:ensure_dir(filename:join(App ++ ["include", "t.hrl"])),
    filelib:ensure_dir(filename:join(App ++ ["src", "t.erl"])),
    filelib:ensure_dir(filename:join(App ++ ["test", "t.erl"])),
    filelib:ensure_dir(filename:join(CApp ++ ["include", "t.erl"])),
    filelib:ensure_dir(filename:join(CApp ++ ["src", "t.erl"])),
    filelib:ensure_dir(filename:join(CApp ++ ["test", "t.erl"])),

    Actual = edt:includes(),
    Expected = [filename:join(App ++ ["include"]),
                filename:join(App ++ ["src"]),
                filename:join(CApp ++ ["include"]),
                filename:join(CApp ++ ["src"]),
                filename:join([PrivDir, "include"])],
    Expected = Actual,
    ok.
test_parse_path(_Config) ->
    Path1 = "/Users/name/working/edt/src/testing.erl",
    {ok, {src, "edt", _}} = edt:parse_path(Path1),

    Path2 = "/Users/name/working/edt/test/testing.erl",
    {ok, {test, "edt", _}} = edt:parse_path(Path2),

    Path3 = "/Users/name/working/edt/testing.erl",
    {error, {Path3, unknown}} = edt:parse_path(Path3),
    ok.

test_outdir(_Config) ->
    Profile = edt:rebar3_profile(),

    Path1 = "/Users/name/working/EDT_1/src/testing.erl",
    Expected1 = "_build/" ++ Profile ++ "/lib/EDT_1/ebin",
    Expected1 = edt:outdir(Path1),

    Path2 = "/Users/name/working/EDT_2/src/testing.erl",
    Expected2 = "_build/" ++ Profile ++ "/lib/EDT_2/ebin",
    Expected2 = edt:outdir(Path2),

    Path3 = "/Users/name/working/EDT_3/test/testing.erl",
    Expected3 = "_build/" ++ Profile ++ "/lib/EDT_3/test",
    Expected3 = edt:outdir(Path3),

    Path4 = "/Users/name/working/_checkouts/EDT_3/src/testing.erl",
    Expected4 = "_checkouts/EDT_3/ebin",
    Expected4 = edt:outdir(Path4),

    Path5 = "/Users/name/working/_checkouts/EDT_3/test/testing.erl",
    Expected5 = "_checkouts/EDT_3/test",
    Expected5 = edt:outdir(Path5),
    ok.

test_file_type(_Config) ->
    Path1 = "/Users/name/working/EDT_1/src/testing.erl",
    src = edt:file_type(Path1),

    Path2 = "/Users/name/working/EDT_1/src/testing.beam",
    beam = edt:file_type(Path2),

    Path3 = "/Users/name/working/EDT_1/src/testing.hrl",
    unknown = edt:file_type(Path3),

    Path4 = "/Users/name/working/EDT_1/src/testing.xxx",
    unknown = edt:file_type(Path4),

    ok.

test_module_name(_Config) ->
    testing = edt:module_name("/Users/name/working/EDT_1/src/testing.erl"),
    ok.

test_get_env(_Config) ->
    none = edt:get_env(testing, none),

    os:putenv("EDT_TESTING", "OS ENV value"),
    "OS ENV value" = edt:get_env(testing, none),
    os:unsetenv("EDT_TESTING"),

    os:putenv("EDT_TESTING", "OS ENV value"),
    application:set_env(edt, testing, 'APP ENV value'),
    'APP ENV value' = edt:get_env(testing, none),
    os:unsetenv("EDT_TESTING"),
    application:unset_env(edt, testing),
    ok.

test_relative_path(_Config) ->
    Path1 = "/Users/name/working/edt/src/testing.erl",
    "edt/src/testing.erl" = edt:relative_path(Path1),

    Path2 = "/Users/name/working/edt/test/testing.erl",
    "edt/test/testing.erl" = edt:relative_path(Path2),

    Path3 = "/Users/name/working/_checkouts/edt/src/testing.erl",
    "_checkouts/edt/src/testing.erl" = edt:relative_path(Path3),

    Path4 = "/Users/name/working/_checkouts/edt/test/testing.erl",
    "_checkouts/edt/test/testing.erl" = edt:relative_path(Path4),

    Path5 = "/Users/name/working/edt/testing.erl",
    {error, {Path5, unknown}} = edt:relative_path(Path5),
    ok.
