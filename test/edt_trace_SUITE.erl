-module(edt_trace_SUITE).

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

init_per_testcase(_TestCase, Config) ->
    {ok, _} = edt_trace:start(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    edt_trace:stop(),
    ok.

all() ->
    [test_trace].

test_trace(_Config) ->
    edt_trace:trace(edt_trace_SUITE, trace_me),
    not_found = edt_trace:trace_result(edt_trace_SUITE, trace_me),

    Args1 = [test_trace, test1],
    Result1 = edt_trace_SUITE:trace_me(test_trace, test1),
    timer:sleep(100),
    TraceResult1 = edt_trace:trace_result(edt_trace_SUITE, trace_me),
    [args, result] =  lists:sort(maps:keys(TraceResult1)),
    #{args := Args1} = TraceResult1,
    #{result := Result1} = TraceResult1,

    Args2 = [test_trace, test2],
    Result2 = edt_trace_SUITE:trace_me(test_trace, test2),
    timer:sleep(100),
    TraceResult2 = edt_trace:trace_result(edt_trace_SUITE, trace_me),
    [args, result] =  lists:sort(maps:keys(TraceResult2)),
    #{args := Args2} = TraceResult2,
    #{result := Result2} = TraceResult2,

    ok.

%% ---------------------------------------------------------
%% Helper functions
%% ---------------------------------------------------------

trace_me(Arg1, Arg2) ->
    {ok, {Arg1, Arg2}}.
