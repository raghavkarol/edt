-module(edt_profile_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include("edt_profile.hrl").

suite() ->
    [{timetrap, {seconds, 5}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    edt_profile:start(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    catch edt_profile:stop(),
    ok.

all() ->
    [
        test_trace1_without_opts,
        test_trace1_with_opts,
        test_trace2,
        test_trace3,
        test_trace4,
        test_summary,
        test_call_graph,
        test_capture
    ].

test_trace1_without_opts(_Config) ->
    edt_profile:trace([{edt_profile_SUITE, one}]),
    one(),
    timer:sleep(100),
    edt_profile_pprint:summary(),
    ok.

test_trace1_with_opts(_Config) ->
    edt_profile:trace([{edt_profile_SUITE, one, '_', #{capture => true, start_context => true}}]),
    one(),
    timer:sleep(100),
    edt_profile_pprint:summary(),
    ok.

test_trace2(_Config) ->
    edt_profile:trace(edt_profile_SUITE, one),
    one(),
    timer:sleep(100),
    edt_profile_pprint:summary(),
    ok.

test_trace3(_Config) ->
    edt_profile:trace(edt_profile_SUITE, one, '_', #{capture => true, start_context => true}),
    one(),
    timer:sleep(100),
    edt_profile_pprint:summary(),
    ok.

test_trace4(_Config) ->
    edt_profile:trace(edt_profile_SUITE, one, '_', #{capture => true, start_context => true}),
    one(),
    timer:sleep(100),
    edt_profile_pprint:summary(),
    ok.

test_summary(_Config) ->
    edt_profile:trace([
        {edt_profile_SUITE, one},
        {edt_profile_SUITE, two},
        {edt_profile_SUITE, three}
    ]),
    one(),
    timer:sleep(100),
    edt_profile_pprint:summary(),
    ok.

test_call_graph(_Config) ->
    Specs = [
        {edt_profile_SUITE, one},
        {edt_profile_SUITE, two},
        {edt_profile_SUITE, three}
    ],
    Opts = #{track_calls => true},
    edt_profile:trace_opts(Specs, Opts),
    one(),
    timer:sleep(100),
    edt_profile_pprint:call_graph(),
    ok.

test_capture(_Config) ->
    Specs = [{edt_profile_SUITE, one, '_', #{capture => true}}],
    Opts = #{track_calls => true, max_calls => 10},
    edt_profile:trace_opts(Specs, Opts),
    one(),
    timer:sleep(100),
    [CRec] = edt_profile:stat_call(edt_profile_SUITE, one),
    #crec{
        module = edt_profile_SUITE,
        func = one
    } = CRec,
    #crec{args = []} = CRec,
    #crec{result = ok} = CRec,
    CRec = edt_profile:stat_call(CRec#crec.id),
    ok.

%% ---------------------------------------------------------
%% Internal functions
%% ---------------------------------------------------------
one() ->
    two().

two() ->
    three().

three() ->
    ok.
