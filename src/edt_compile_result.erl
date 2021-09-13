%%
%% Copyright 2020 Raghav Karol.
%%
-module(edt_compile_result).

-export([format/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec format1({'error' | 'warning', edt:compile_msgs()}, 'text' | 'json') -> binary().
format1({Type, Msgs}, Format) ->
    Type1 =
        case Type of
            warning ->
                "Warning";
            error ->
                "Error"
        end,
    Result = [
        [Path, ":", edt_lib:to_string(LineNo), ":", " ", Type1, ": ", Mod:format_error(Msg)]
     || {Path, W1} <- Msgs,
        {LineNo, Mod, Msg} <- W1
    ],
    case Format of
        json ->
            lists:map(fun(R) -> iolist_to_binary(R) end, Result);
        text ->
            Result1 = string:join(Result, "\n"),
            iolist_to_binary([Result1, "\n"])
    end.

%% ---------------------------------------------------------
%% API
%% ---------------------------------------------------------
format({ok, {Path, _Module, []}}, text) ->
    ["compiled ", Path];
format({ok, {Path, _Module, Warnings}}, text) ->
    Warnings1 = [$\n, format1({warning, Warnings}, text)],
    ["compiled ", Path, Warnings1];
format({ok, {Path, _Module, Warnings}}, json) ->
    Path1 = edt_lib:to_binary(Path),
    Warnings1 = format1({warning, Warnings}, json),
    #{
        path => Path1,
        warnings => Warnings1
    };
format({error, {Path, Errors, []}}, text) ->
    Errors1 = format1({error, Errors}, text),
    ["ERROR ", Path, " ", $\n, Errors1];
format({error, {Path, Errors, Warnings}}, text) ->
    Errors1 = format1({error, Errors}, text),
    Warnings1 = ["Warnings", $\n, format1({warning, Warnings}, text)],
    ["ERROR ", Path, " ", $\n, Errors1, $\n, Warnings1];
format({error, {Path, Errors, Warnings}}, json) ->
    Path1 = edt_lib:to_binary(Path),
    Errors1 = format1({error, Errors}, json),
    Warnings1 = format1({warning, Warnings}, json),
    #{
        path => Path1,
        errors => Errors1,
        warnings => Warnings1
    };
format({error, {Path, unknown}}, text) ->
    ["ignored ", Path, " do not know how to handle it"];
format({error, {Path, unknown}}, json) ->
    Path1 = edt_lib:to_binary(Path),
    #{
        path => Path1,
        message => iolist_to_binary(["ignored ", Path, " do not know how to handle it"])
    }.

%% ---------------------------------------------------------
%% Test
%% ---------------------------------------------------------
-ifdef(TEST).
format_warnings_test() ->
    Msgs = [
        {"_checkouts/edt/src/edt.erl", [
            {36, erl_lint, {unused_var, 'Errors'}},
            {36, erl_lint, {unused_var, 'Warnings'}},
            {194, sys_core_fold, no_clause_match},
            {194, sys_core_fold, nomatch_guard}
        ]}
    ],
    Expected =
        <<"_checkouts/edt/src/edt.erl:36: Warning: variable 'Errors' is unused", "\n",
            "_checkouts/edt/src/edt.erl:36: Warning: variable 'Warnings' is unused", "\n",
            "_checkouts/edt/src/edt.erl:194: Warning: no clause will ever match", "\n",
            "_checkouts/edt/src/edt.erl:194: Warning: the guard for this clause evaluates to 'false'",
            "\n">>,
    Actual = format1({warning, Msgs}, text),
    Expected = Actual,
    ok.

format_errors_test() ->
    Msgs = [
        {"_checkouts/edt/src/edt.erl", [
            {36, erl_lint, {unused_var, 'Errors'}},
            {36, erl_lint, {unused_var, 'Warnings'}},
            {194, sys_core_fold, no_clause_match},
            {194, sys_core_fold, nomatch_guard}
        ]}
    ],
    Expected =
        <<"_checkouts/edt/src/edt.erl:36: Error: variable 'Errors' is unused", "\n",
            "_checkouts/edt/src/edt.erl:36: Error: variable 'Warnings' is unused", "\n",
            "_checkouts/edt/src/edt.erl:194: Error: no clause will ever match", "\n",
            "_checkouts/edt/src/edt.erl:194: Error: the guard for this clause evaluates to 'false'",
            "\n">>,
    Actual = format1({error, Msgs}, text),
    Expected = Actual,
    ok.

report_test_() ->
    Msgs = [
        {"testing.erl", [
            {36, erl_lint, {unused_var, 'Errors'}},
            {36, erl_lint, {unused_var, 'Warnings'}},
            {194, sys_core_fold, no_clause_match},
            {194, sys_core_fold, nomatch_guard}
        ]}
    ],
    [
        {"compile ok text", fun() ->
            Expected = ["compiled ", "testing.erl"],
            Actual = format({ok, {"testing.erl", testing, []}}, text),
            ?assertEqual(Expected, Actual)
        end},
        {"compile ok json", fun() ->
            Expected = #{
                path => <<"testing.erl">>,
                warnings => []
            },
            Actual = format({ok, {"testing.erl", testing, []}}, json),
            ?assertEqual(Expected, Actual)
        end},
        {"compile with warnings text", fun() ->
            Expected = [
                "compiled ",
                "testing.erl",
                [
                    $\n,
                    <<"testing.erl:36: Warning: variable 'Errors' is unused", "\n",
                        "testing.erl:36: Warning: variable 'Warnings' is unused", "\n",
                        "testing.erl:194: Warning: no clause will ever match", "\n",
                        "testing.erl:194: Warning: the guard for this clause evaluates to 'false'",
                        "\n">>
                ]
            ],
            Actual = format({ok, {"testing.erl", testing, Msgs}}, text),
            ?assertEqual(Actual, Expected)
        end},
        {"compile with warnings json", fun() ->
            Expected = #{
                path => <<"testing.erl">>,
                warnings => [
                    <<"testing.erl:36: Warning: variable 'Errors' is unused">>,
                    <<"testing.erl:36: Warning: variable 'Warnings' is unused">>,
                    <<"testing.erl:194: Warning: no clause will ever match">>,
                    <<"testing.erl:194: Warning: the guard for this clause evaluates to 'false'">>
                ]
            },
            Actual = format({ok, {"testing.erl", testing, Msgs}}, json),
            ?assertEqual(Actual, Expected)
        end},
        {"compile error text", fun() ->
            Expected = [
                "ERROR ",
                "testing1",
                " ",
                $\n,
                <<"testing.erl:36: Error: variable 'Errors' is unused", "\n",
                    "testing.erl:36: Error: variable 'Warnings' is unused", "\n",
                    "testing.erl:194: Error: no clause will ever match" "\n",
                    "testing.erl:194: Error: the guard for this clause evaluates to 'false'", "\n">>
            ],
            Actual = format({error, {"testing1", Msgs, []}}, text),
            ?assertEqual(Expected, Actual)
        end},
        {"compile error json", fun() ->
            Expected = #{
                path => <<"testing1">>,
                warnings => [],
                errors =>
                    [
                        <<"testing.erl:36: Error: variable 'Errors' is unused">>,
                        <<"testing.erl:36: Error: variable 'Warnings' is unused">>,
                        <<"testing.erl:194: Error: no clause will ever match">>,
                        <<"testing.erl:194: Error: the guard for this clause evaluates to 'false'">>
                    ]
            },
            Actual = format({error, {"testing1", Msgs, []}}, json),
            ?assertEqual(Expected, Actual)
        end},
        {"compile error with warnings text", fun() ->
            Expected = [
                "ERROR ",
                "testing1",
                " ",
                $\n,
                <<"\n">>,
                $\n,
                [
                    "Warnings",
                    $\n,
                    <<"testing.erl:36: Warning: variable 'Errors' is unused", "\n",
                        "testing.erl:36: Warning: variable 'Warnings' is unused", "\n",
                        "testing.erl:194: Warning: no clause will ever match", "\n",
                        "testing.erl:194: Warning: the guard for this clause evaluates to 'false'",
                        "\n">>
                ]
            ],
            Actual = format({error, {"testing1", [], Msgs}}, text),
            ?assertEqual(Expected, Actual)
        end},
        {"compile error with warnings json", fun() ->
            Expected = #{
                path => <<"testing1">>,
                errors => [],
                warnings =>
                    [
                        <<"testing.erl:36: Warning: variable 'Errors' is unused">>,
                        <<"testing.erl:36: Warning: variable 'Warnings' is unused">>,
                        <<"testing.erl:194: Warning: no clause will ever match">>,
                        <<"testing.erl:194: Warning: the guard for this clause evaluates to 'false'">>
                    ]
            },
            Actual = format({error, {"testing1", [], Msgs}}, json),
            ?assertEqual(Expected, Actual)
        end},
        {"ignored path text", fun() ->
            Expected = ["ignored ", "testing1.erl", " do not know how to handle it"],
            Actual = format({error, {"testing1.erl", unknown}}, text),
            ?assertEqual(Expected, Actual)
        end},
        {"ignored path json", fun() ->
            Expected = #{
                path => <<"testing1.erl">>,
                message => <<"ignored testing1.erl do not know how to handle it">>
            },
            Actual = format({error, {"testing1.erl", unknown}}, json),
            ?assertEqual(Expected, Actual)
        end}
    ].
-endif.
