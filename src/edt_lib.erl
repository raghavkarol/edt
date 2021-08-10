%%
%% Copyright 2020 Raghav Karol.
%%
-module(edt_lib).

-export([to_atom/1,
         to_binary/1,
         to_integer/1,
         to_string/1,
         to_boolean/1]).

-export([retry/2,
         retry/3,
         retry_until/1,
         retry_until/2]).

-export([parse_rebar3_profile/1,
         is_eunit_generator/1,
         which_test/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

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
    list_to_binary(V);
to_binary(V) when is_integer(V) ->
    integer_to_binary(V);
to_binary(V) when is_atom(V) ->
    atom_to_binary(V, 'utf8').

to_boolean("true") ->
    true;
to_boolean(<<"true">>) ->
    true;
to_boolean(1) ->
    true;
to_boolean(true) ->
    true;
to_boolean("1") ->
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

-spec is_eunit_generator(function()) -> boolean().
is_eunit_generator(Func) ->
    string:find(atom_to_list(Func), "_", trailing) == "_".

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
     end].

which_test_test() ->
    eunit = which_test("checkouts/src/edt.erl"),
    ct = which_test("checkouts/test/edt_SUITE.erl"),
    ok.

is_eunit_generator_test() ->
    true = is_eunit_generator(a_generator_),
    false = is_eunit_generator(a_simple_test).

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
    true = to_boolean("1"),
    true = to_boolean(1),
    true = to_boolean(true),
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

parse_rebar3_profile_test() ->
    "test" = parse_rebar3_profile("rebar3 as test shell --apps edt"),
    "default" = parse_rebar3_profile("rebar3 shell --apps edt"),

    {error, Error} = parse_rebar3_profile("erl"),
    {not_parseable, {nomatch,"erl"}} = Error,
    ok.

-endif.
