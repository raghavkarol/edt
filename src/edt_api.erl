-module(edt_api).

-export([compile/1,
         test/1]).

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
compile(Path) when is_list(Path);
                   is_binary(Path) ->
    Result = edt:compile(Path),
    Report = edt_lib:report(Result),
    edt_out:stdout(Report),
    ok.

%% @doc
%%
%% Discover and the run the tests in Module.
%%
%% supported test types are 'eunit' and 'ct'.
%%
%% @end
test(Module) when is_atom(Module) ->
    Path = edt:source_path(Module),
    Type = edt_lib:which_test(Path),
    Result = edt:compile(Path),
    case Result of
        {error, _} ->
            Report = edt_lib:report(Result),
            edt_out:stdout(Report),
            ok;
        {ok, _} ->
            edt:test(Type, Module),
            ok
    end,
    ok.
