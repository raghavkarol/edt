-module(edt_lib).

-export([to_atom/1,
         to_binary/1,
         to_integer/1,
         to_string/1,
         to_boolean/1]).

-export([format/2,
         report/1]).

-export([retry/2,
         retry/3,
         retry_until/1,
         retry_until/2]).

-export([parse_rebar3_profile/1,
         which_test/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec format(Msgs :: [{Path::edt:path(),
                       [{LineNum::integer(),
                         module(),
                         atom() | {atom(), any()}}]}],
             'error'|'warning') -> binary().
format(Msgs, Type) ->
    Type1 =
        case Type of
            warning ->
                "Warning";
            error ->
                "Error"
        end,
    Result = [ [Path, ":", edt_lib:to_string(LineNo), ":", " ", Type1, ": ", Mod:format_error(Msg)] ||
                 {Path, W1} <- Msgs,
                 {LineNo, Mod, Msg} <- W1 ],
    Result1 = string:join(Result, "\n"),
    iolist_to_binary([Result1, "\n"]).

-spec report(edt:compile_ret()) -> iolist().
report({ok, {Path, _Module, Warnings}}) ->
    ["compiled ", Path,
     if Warnings /= []->
             [$\n, edt_lib:format(Warnings, warning)];
        true ->
             []
     end];
report({error, {Path, Errors, Warnings}}) ->
    ["ERROR ", Path, " ", $\n,
     edt_lib:format(Errors, error),
     $\n,
     if Warnings /= []->
             ["Warnings", $\n, edt_lib:format(Warnings, warning)];
        true ->
             []
     end];
report({error, {Path, unknown}})  ->
    ["ignored ", Path, " do not know how to handle it"].

to_atom(V)
  when is_binary(V) ->
    V1 = binary_to_atom(V, 'utf8'),
    to_atom(V1);
to_atom(V)
  when is_integer(V) ->
    V1 = integer_to_list(V),
    to_atom(V1);
to_atom(V)
  when is_list(V) ->
    list_to_atom(V);
to_atom(V)
  when is_atom(V) ->
    V.

to_binary(V)
  when is_binary(V) ->
    V;
to_binary(V) when is_list(V) ->
    list_to_binary(V).

to_boolean("true") ->
    true;
to_boolean(<<"true">>) ->
    true;
to_boolean(1) ->
    true;
to_boolean(_) ->
    false.

to_integer(V) when is_list(V) ->
    list_to_integer(V);
to_integer(V) when is_integer(V)->
    V.

to_string(V) when is_binary(V) ->
    binary_to_list(V);
to_string(V) when is_integer(V) ->
    integer_to_list(V);
to_string(V) when is_atom(V) ->
    atom_to_list(V);
to_string(V) when is_list(V) ->
    V.

retry_until(F) ->
    retry(F, true).

retry_until(F, Timeout) ->
    retry(F, true, Timeout).

retry(F, Expected) ->
    retry(F, Expected, timer:seconds(2)).

retry(F, Expected, Timeout) ->
    retry(F, Expected, '_', Timeout).

retry(_, Expected, Actual, Timeout)
  when Timeout =< 0 ->
    error({retry_timeout,
           {expected, Expected, actual, Actual}});
retry(F, Expected, _, Timeout) ->
    SleepTime = 100,
    case F() of
        Expected ->
            Expected;
        Actual ->
            timer:sleep(SleepTime),
            retry(F, Expected, Actual, Timeout-SleepTime)
    end.


-spec which_test(Path :: edt:path()) -> 'ct'|'eunit'.
which_test(Path) ->
    Path1 = to_binary(Path),
    case re:run(Path1, <<"_SUITE.erl">>, [{capture, none}]) of
        match ->
            ct;
        _ ->
            eunit
    end.

parse_rebar3_profile(Cmd) ->
    case re:run(Cmd, "rebar3\s+shell|as\s+(.+?)\s+shell", [{capture, all, list}]) of
        {match, [_, Profile]} ->
            Profile;
        {match, [_]} ->
            "default";
        Result ->
            {error, {not_parseable, {Result, Cmd}}}
    end.

%% ---------------------------------------------------------
%% Unit tests
%% ---------------------------------------------------------
-ifdef(TEST).

retry_test_() ->
    [fun() ->
             1 = retry(fun() -> 1 end, 1)
     end,
     fun() ->
             {'EXIT', {{retry_timeout,{expected,1,actual,2}}, _}} = (catch retry(fun() -> 2 end, 1, 100))
     end,
     fun() ->
             true = retry_until(fun() -> true end)
     end,
     fun() ->
             true = retry_until(fun() -> true end, 1)
     end,
     fun() ->
             {'EXIT', {{retry_timeout,{expected,true,actual,false}}, _}} = (catch retry_until(fun() -> false end, 1))
     end
    ].

which_test_test() ->
    eunit = which_test("checkouts/src/edt.erl"),
    ct = which_test("checkouts/test/edt_SUITE.erl"),
    ok.

to_atom_test() ->
    '1' = to_atom(1),
    testing = to_atom(testing),
    testing = to_atom("testing"),
    testing = to_atom(<<"testing">>),
    ok.

to_binary_test() ->
    <<"test">> = to_binary(<<"test">>),
    <<"test">> = to_binary("test"),
    ok.

to_boolean_test() ->
    true = to_boolean("true"),
    true = to_boolean(<<"true">>),
    true = to_boolean(1),
    false = to_boolean(false),
    ok.

to_integer_test() ->
    1 = to_integer("1"),
    1 = to_integer(1),
    ok.

to_string_test() ->
    "one" = to_string(<<"one">>),
    "1" = to_string(1),
    "1" = to_string('1'),
    "one" = to_string("one"),
    ok.

format_warnings_test() ->
    Msgs = [{"_checkouts/edt/src/edt.erl",
             [{36,erl_lint,{unused_var,'Errors'}},
              {36,erl_lint,{unused_var,'Warnings'}},
              {194,sys_core_fold,no_clause_match},
              {194,sys_core_fold,nomatch_guard}]}],
    Expected =  <<"_checkouts/edt/src/edt.erl:36: Warning: variable 'Errors' is unused", "\n",
                  "_checkouts/edt/src/edt.erl:36: Warning: variable 'Warnings' is unused", "\n",
                  "_checkouts/edt/src/edt.erl:194: Warning: no clause will ever match", "\n",
                  "_checkouts/edt/src/edt.erl:194: Warning: the guard for this clause evaluates to 'false'", "\n">>,
    Actual = format(Msgs, warning),
    Expected = Actual,
    ok.

format_errors_test() ->
    Msgs = [{"_checkouts/edt/src/edt.erl",
             [{36,erl_lint,{unused_var,'Errors'}},
              {36,erl_lint,{unused_var,'Warnings'}},
              {194,sys_core_fold,no_clause_match},
              {194,sys_core_fold,nomatch_guard}]}],
    Expected =  <<"_checkouts/edt/src/edt.erl:36: Error: variable 'Errors' is unused", "\n",
                  "_checkouts/edt/src/edt.erl:36: Error: variable 'Warnings' is unused", "\n",
                  "_checkouts/edt/src/edt.erl:194: Error: no clause will ever match", "\n",
                  "_checkouts/edt/src/edt.erl:194: Error: the guard for this clause evaluates to 'false'", "\n">>,
    Actual = format(Msgs, error),
    Expected = Actual,
    ok.

report_test() ->
    Msgs = [{"testing.erl",
             [{36,erl_lint,{unused_var,'Errors'}},
              {36,erl_lint,{unused_var,'Warnings'}},
              {194,sys_core_fold,no_clause_match},
              {194,sys_core_fold,nomatch_guard}]}],
    Expected1 = ["compiled ","testing.erl",
                [$\n,
                 <<"testing.erl:36: Warning: variable 'Errors' is unused", "\n",
                   "testing.erl:36: Warning: variable 'Warnings' is unused", "\n",
                   "testing.erl:194: Warning: no clause will ever match", "\n",
                   "testing.erl:194: Warning: the guard for this clause evaluates to 'false'", "\n">>]],
    Resp1 = {"testing.erl", testing, Msgs},
    Actual1 = report({ok, Resp1}),
    Actual1 = Expected1,

    Resp2 = {"testing.erl", testing, []},
    Actual2 = report({ok, Resp2}),
    Expected2 = ["compiled ","testing.erl",[]],
    Actual2 = Expected2,

    Actual3 = report({error, {"testing1", Msgs, []}}),
    Expected3 = ["ERROR ","testing1"," ",$\n,
                 <<"testing.erl:36: Error: variable 'Errors' is unused", "\n",
                   "testing.erl:36: Error: variable 'Warnings' is unused", "\n",
                   "testing.erl:194: Error: no clause will ever match" "\n",
                   "testing.erl:194: Error: the guard for this clause evaluates to 'false'", "\n">>,
                 $\n,
                 []],
    Expected3 = Actual3,

    Actual4 = report({error, {"testing1", [], Msgs}}),
    Expected4 = ["ERROR ","testing1"," ", $\n,
                 <<"\n">>, $\n,
                 ["Warnings",$\n,
                  <<"testing.erl:36: Warning: variable 'Errors' is unused", "\n",
                    "testing.erl:36: Warning: variable 'Warnings' is unused", "\n",
                    "testing.erl:194: Warning: no clause will ever match", "\n",
                    "testing.erl:194: Warning: the guard for this clause evaluates to 'false'", "\n">>]],
    Actual4 = Expected4,

    Actual5 = report({error, {"testing1.erl", unknown}}),
    Expected5 = ["ignored ", "testing1.erl", " do not know how to handle it"],
    Actual5 = Expected5,
    ok.

parse_rebar3_profile_test() ->
    "test" = parse_rebar3_profile("rebar3 as test shell --apps edt"),
    "default" = parse_rebar3_profile("rebar3 shell --apps edt"),

    {error, Error} = parse_rebar3_profile("erl"),
    {not_parseable, {nomatch,"erl"}} = Error,
    ok.

-endif.
