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

init_per_testcase(test_start_link, Config) ->
    Config;
init_per_testcase(_TestCase, Config) ->
    {ok, _Pid} = edt_post_action:start(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    catch edt_post_action:stop(),
    ok.

all() ->
    [
        test_async_event,
        test_crud,
        test_start_link,
        test_sync_event,
        test_post_action_stop,
        test_post_action_error
    ].

test_crud(_Config) ->
    Fun1 = fun() -> ok end,
    Fun2 = {?MODULE, mfa_dummy, []},
    edt_post_action:add(test1, Fun1),
    edt_post_action:add(test2, Fun2),

    [{test1, Fun1}, {test2, Fun2}] = edt_post_action:list(),
    edt_post_action:delete(test1),
    [{test2, Fun2}] = edt_post_action:list(),
    edt_post_action:delete(test2),
    [] = edt_post_action:list(),

    ok.

test_async_event(_Config) ->
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
    [] = ets:tab2list(Table),
    ok = edt_post_action:event(testing),
    Expected = [test1, test2, test3],
    FunActual =
        fun() ->
            lists:sort([Name || {Name, _} <- ets:tab2list(Table)])
        end,
    edt_lib:retry(FunActual, Expected),
    ok.

test_sync_event(_Config) ->
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
    edt_post_action:add(test4, {?MODULE, mfa_ets_insert, [Table, test4, Now]}),
    Expected = [
        {ok, {test1, true}},
        {ok, {test2, true}},
        {ok, {test3, true}},
        {ok, {test4, true}}
    ],
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
    edt_post_action:add(test4, {?MODULE, mfa_throw, [stop]}),
    Expected = [
        {ok, {stop, test1}},
        {error, {stop, test2}},
        {error, {stop, test3}},
        {error, {stop, test4}}
    ],
    Actual = edt_post_action:sync_event(testing),
    Expected = Actual,
    ok.

test_post_action_error(_Config) ->
    Fun = fun() -> error(fail) end,
    edt_post_action:add(test1, Fun),
    edt_post_action:add(test2, Fun),
    edt_post_action:add(test3, Fun),
    edt_post_action:add(test4, {?MODULE, mfa_error, [fail]}),
    Expected = [
        {error, {test1, {error, fail}}},
        {error, {test2, {error, fail}}},
        {error, {test3, {error, fail}}},
        {error, {test4, {error, fail}}}
    ],
    Actual = edt_post_action:sync_event(testing),
    Expected = Actual,
    ok.

test_start_link(_Config) ->
    {ok, _Pid} = edt_post_action:start_link(),
    ok.

mfa_dummy() ->
    ok.

mfa_ets_insert(Table, Name, Now) ->
    ets:insert(Table, {Name, erlang:system_time(microsecond) - Now}).

mfa_throw(Msg) ->
    throw(Msg).

mfa_error(Reason) ->
    error(Reason).
