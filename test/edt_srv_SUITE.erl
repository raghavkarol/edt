-module(edt_srv_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("edt_srv.hrl").

suite() ->
    [{timetrap, {seconds, 30}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    {ok, Home} = ct_helper:setup_test_data(Config),
    application:set_env(edt, ignore_regex, ""),
    application:set_env(edt, auto_process, "false"),
    edt_srv:start(Home),
    wait(),
    [{home, Home} | Config].

end_per_testcase(_TestCase, _Config) ->
    edt_srv:stop(),
    ok.

all() ->
    [
        test_ignore_regex,
        test_changes,
        test_changes_auto_process,
        test_changes_process
    ].

test_changes(Config) ->
    Home = ?config(home, Config),
    [] = edt_srv:changes(),
    Path1 = Home ++ "/src/test_compile_ok.erl",
    update_file(Path1),
    edt_lib:retry_until(
        fun() ->
            case edt_srv:changes() of
                [_] ->
                    true;
                _ ->
                    false
            end
        end
    ),
    edt_lib:retry_until(
        fun() ->
            case edt_srv:changes() of
                [_] ->
                    true;
                _ ->
                    false
            end
        end
    ),
    Path2 = Home ++ "/src/test_compile_fail.erl",
    update_file(Path2),
    edt_lib:retry_until(
        fun() ->
            case edt_srv:changes() of
                [_, _] ->
                    true;
                _ ->
                    false
            end
        end
    ),
    remove_file(Path2),
    edt_lib:retry_until(
        fun() ->
            case edt_srv:changes() of
                [_] ->
                    true;
                _ ->
                    false
            end
        end
    ),
    ok.

test_changes_auto_process(Config) ->
    Home = ?config(home, Config),
    application:set_env(edt, auto_process, "true"),
    edt_lib:retry(
        fun edt_srv:stats/0,
        #stats{change = 0, ignore = 0, handle = 0}
    ),
    Path = Home ++ "/src/test_compile_ok.erl",
    update_file(Path),
    edt_lib:retry_until(
        fun() ->
            #stats{handle = H} = edt_srv:stats(),
            H > 0
        end
    ),
    ok.

test_changes_process(Config) ->
    Home = ?config(home, Config),
    <<"">> = edt:ignore_regex(),
    edt_lib:retry(
        fun edt_srv:stats/0,
        #stats{change = 0, ignore = 0, handle = 0}
    ),
    Path = Home ++ "/src/test_compile_ok.erl",
    update_file(Path),
    edt_lib:retry_until(fun() ->
        #stats{change = C} = edt_srv:stats(),
        C > 0
    end),
    edt_srv:process_changes(),
    edt_lib:retry_until(
        fun() ->
            #stats{handle = H} = edt_srv:stats(),
            H > 0
        end
    ),
    edt_srv:process_changes(),
    ok.

test_ignore_regex(Config) ->
    Home = ?config(home, Config),
    application:set_env(edt, ignore_regex, <<"ignore">>),
    <<"ignore">> = edt:ignore_regex(),
    edt_lib:retry(
        fun edt_srv:stats/0,
        #stats{change = 0, ignore = 0, handle = 0}
    ),
    Path = Home ++ "/src/test_compile_ok_ignore.erl",
    update_file(Path),
    edt_lib:retry_until(
        fun() ->
            #stats{ignore = I} = edt_srv:stats(),
            I > 0
        end
    ),
    ok.

%% ---------------------------------------------------------
%% Internal functions
%% ---------------------------------------------------------
update_file(Path) ->
    "" = os:cmd("touch " ++ Path),
    ok.

remove_file(Path) ->
    "" = os:cmd("rm " ++ Path),
    ok.

%%
%% Required on linux to allow edt srv to startup and subscribe to
%% receive events
%%
wait() ->
    receive
        _ ->
            ok
    after 100 ->
        ok
    end.
