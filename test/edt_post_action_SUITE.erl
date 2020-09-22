-module(edt_post_action_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap, {seconds, 5}}].

init_per_suite(Config) ->
    edt_out:start(),
    Config.

end_per_suite(_Config) ->
    edt_out:stop(),
    ok.

init_per_testcase(_TestCase, Config) ->
    {ok, _Pid} = edt_post_action:start(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    edt_post_action:stop(),
    ok.

all() ->
    [test_crud,
     test_event,
     test_post_action_stop].

test_crud(_Config) ->
    Fun1 = fun() -> ok end,
    Fun2 = fun() -> ok end,
    edt_post_action:add(test1, Fun1),
    edt_post_action:add(test2, Fun2),

    [test1, test2] = edt_post_action:list(),
    edt_post_action:delete(test1),
    [test2] = edt_post_action:list(),
    edt_post_action:delete(test2),
    [] = edt_post_action:list(),

    edt_post_action:add(eunit, {eunit, kernel}),
    edt_post_action:add(ct, {ct, kernel_SUITE}),
    [ct, eunit] = edt_post_action:list(),

    ok.

test_event(_Config) ->
    Table = ets:new(test_event, [public, ordered_set]),
    Now = erlang:system_time(microsecond),
    Fun =
        fun(Name) ->
                fun() ->
                        ets:insert(Table, {Name, erlang:system_time(microsecond) - Now})
                end
        end,
    edt_post_action:add(test1, Fun(test1)),
    edt_post_action:add(test2, Fun(test2)),
    edt_post_action:add(test3, Fun(test3)),
    Expected = [{ok, {test1, true}},
                {ok, {test2, true}},
                {ok, {test3, true}}],
    Actual = edt_post_action:sync_event(testing),
    Expected = Actual,
    InsertTs = [Ts || {_Name, Ts} <- ets:tab2list(Table)],
    InsertTs = lists:sort(InsertTs),
    ok.

test_post_action_stop(_Config) ->
    Fun = fun() -> throw(stop) end,
    edt_post_action:add(test1, Fun),
    edt_post_action:add(test2, Fun),
    edt_post_action:add(test3, Fun),
    Expected = [{ok, {stop, test1}},
                {error, {stop, test2}},
                {error, {stop, test3}}],
    Actual = edt_post_action:sync_event(testing),
    Expected = Actual,
    ok.
