%%
%% Copyright 2020 Raghav Karol.
%%
-module(edt_api).

-export([
    compile/1,
    test/1, test/2, test/3
]).
%% ---------------------------------------------------------
%% API
%% ---------------------------------------------------------

%% @doc
%%
%% Compile and reload a module for given a module name of a source
%% path
%%
%% @end
compile(Module) when is_atom(Module) ->
    Path = edt:source_path(Module),
    compile(Path);
compile(Path) when
    is_list(Path);
    is_binary(Path)
->
    Result = edt:compile(Path),
    case Result of
        {ok, _} ->
            Report = edt_compile_result:format(Result, text),
            edt_out:stdout(Report),
            ok;
        {error, _} = E ->
            E
    end.

%%
%% @doc Discover and the run the tests in Module. If TestCase is not
%% undefined, run only that test case. Supports discovery of eunit and
%% ct test types.
%%
%% Module is the module containing tests .e.g. edt_lib or edt_SUITE
%% TestCase is a test case function
%%
%%  eunit :: https://erlang.org/doc/man/eunit.html#test-2
%%  ct    :: https://erlang.org/doc/man/ct.html#run_test-1
%%
test(Module, TestCase, Opts) ->
    Path = edt:source_path(Module),
    Type = edt_lib:which_test(Path),
    Result = edt:compile(Path),
    case Result of
        {ok, _} ->
            edt:test(Type, Module, TestCase, Opts);
        {error, _} = E ->
            Report = edt_compile_result:format(Result, text),
            edt_out:stdout(Report),
            E
    end.

test(Module, TestCase) ->
    test(Module, TestCase, []).

test(Module) ->
    test(Module, undefined, []).
