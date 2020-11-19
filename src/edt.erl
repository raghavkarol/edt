%%
%% API for Erlang Development Tools (edt)
%%
-module(edt).

-export([compile/1,
         compile/2,
         reload/1,
         test/2]).

-export([file_type/1,
         get_env/1,
         get_env/2,
         includes/0]).

-export([
         ct_groups/2,
         module_name/1,
         outdir/1,
         parse_path/1,
         relative_path/1,
         source_path/1]).

-export([auto_process/0,
         enable_http_server/0,
         home/0,
         http_port/0,
         ignore_regex/0,
         rebar3_profile/0]).
%% Types
-type compile_ret() :: {ok, {Path :: path(), module(), list()}}
                     | {error, {Errors ::list(),
                                Warnings :: list()}}.

-type path() :: string() | binary().

%% ---------------------------------------------------------
%% API
%% ---------------------------------------------------------
-spec compile(Path:: path()) -> compile_ret().
compile(Path) ->
    compile(Path, []).

-spec compile(Path:: path(), Opts :: list()) -> compile_ret().
compile(Path, Opts) ->
    case parse_path(Path) of
        {error, {Path, unknown}} = E  ->
            E;
        {ok, _} ->
            Opts1 = Opts ++ compile_opts(Path),
            case compile:file(Path, Opts1) of
                {ok, Module, Warnings} ->
                    case proplists:is_defined(strong_validation, Opts) of
                        true ->
                            ok;
                        false ->
                            ensure_code_path(Path)
                    end,
                    reload(Module),
                    {ok, {Path, Module, Warnings}};
                {error, Errors, Warnings} ->
                    {error, {Path, Errors, Warnings}}
            end
    end.

reload(Module) ->
    code:purge(Module),
    code:load_file(Module).

test(eunit, Module) ->
    eunit(Module);
test(ct, {Module, TestCase}) ->
    ct(Module, TestCase);
test(ct, Module) ->
    ct(Module).

compile_opts(Path) ->
    Includes = [{i, I} || I <- includes()],
    OutDir = [{outdir, outdir(Path)}],
    [export_all, debug_info, {d, 'TEST'}, return] ++ Includes ++ OutDir.

eunit_opts() ->
    [no_tty, {report, {unite_compact, [profile]}}].

includes() ->
    Home = home(),
    Includes = [filename:join([Home, "include"])] ++
        filelib:wildcard([Home, "/_checkouts/*/src"]) ++
        filelib:wildcard([Home, "/_checkouts/*/include"]) ++
        filelib:wildcard([Home, "/_build/", rebar3_profile() ,"/lib/*/src"]) ++
        filelib:wildcard([Home, "/_build/", rebar3_profile() ,"/lib/*/include"]),
    lists:usort(Includes).

outdir(Path) ->
    case parse_path(Path) of
        {ok, {{src, checkouts}, App, _}} ->
            filename:join(["_checkouts", App, "ebin"]);
        {ok, {{test, checkouts}, App, _}} ->
            filename:join(["_checkouts", App, "test"]);
        {ok, {src, App, _}} ->
            filename:join(["_build", rebar3_profile(), "lib", App, "ebin"]);
        {ok, {test, App, _}} ->
            filename:join(["_build", rebar3_profile(), "lib", App, "test"])
    end.

%% ---------------------------------------------------------
%% Application configuration
%% ---------------------------------------------------------
rebar3_profile() ->
    os:getenv("REBAR3_PROFILE", "default").

home() ->
    {ok, Dir} = file:get_cwd(),
    edt:get_env(home, Dir).

ignore_regex() ->
    edt_lib:to_binary(edt:get_env(ignore_regex, "")).

auto_process() ->
    edt_lib:to_boolean(edt:get_env(auto_process, "true")).

http_port() ->
    edt_lib:to_integer(edt:get_env(http_port, "65000")).

enable_http_server() ->
    edt_lib:to_boolean(edt:get_env(enable_http_server, "0")).


%% ---------------------------------------------------------
%% Internal Functions
%% ---------------------------------------------------------

%% @doc
%%
%% Parses a path for an erlang source or test file and returns
%% information about it's source directory and OTP App that it belongs
%% to.
%%
%% @end
-spec parse_path(string) -> {ok, {src|test|{src, checkouts}|{test, checkouts},
                                  string(),
                                  string()}} |
                            {error, {unknown, string()}}.
parse_path(Path) ->
    Path1 = filename:absname(Path),
    Tokens = lists:reverse(filename:split(Path1)),
    FunPrefix =
        fun(V) ->
                filename:join(lists:reverse(V))
        end,
    case Tokens of
        [_, "src", App, "_checkouts"|Rest] ->
            {ok, {{src, checkouts}, App, FunPrefix(Rest)}};
        [_, "test", App, "_checkouts"|Rest] ->
            {ok, {{test, checkouts}, App, FunPrefix(Rest)}};
        [_, "src", App|Rest] ->
            {ok, {src, App, FunPrefix(Rest)}};
        [_, "test", App|Rest] ->
            {ok, {test, App, FunPrefix(Rest)}};
        _ ->
            {error, {Path, unknown}}
    end.

source_path(Module) ->
    Props = Module:module_info(compile),
    proplists:get_value(source, Props).

relative_path(Path) ->
    case parse_path(Path) of
        {error, _} = E ->
            E;
        {ok, {_, _, Prefix}} ->
            Result1 = string:replace(Path, Prefix, "", leading),
            Result2 = string:replace(Result1, "/", "", leading),
            lists:flatten(Result2)
    end.

file_type(Path) ->
    case filename:extension(Path) of
        ".beam" ->
            beam;
        ".erl" ->
            src;
        _ ->
            unknown
    end.

module_name(Path) ->
    N1 = filename:basename(Path),
    N2 = filename:rootname(N1),
    list_to_atom(N2).

ensure_code_path(Path) ->
    OutDir = edt:outdir(Path),
    OutDir1 = filename:absname(OutDir),
    code:add_patha(OutDir1).

eunit(Module) ->
    Opts = eunit_opts(),
    eunit:test(Module, Opts).

ct(Module) ->
    ct(Module, undefined).

ct(Module, TestCase) ->
    LogDir = "./_build/test/logs/",
    Dir = filename:dirname(source_path(Module)),
    Opts = [{auto_compile, false},
            {dir, Dir},
            {logdir, LogDir}]
        ++ edt:ct_groups(Module, TestCase)
        ++ [{suite, Module} || Module /= undefined ]
        ++ [{testcase, TestCase} || TestCase /= undefined ],
    filelib:ensure_dir(LogDir),
    ct:run_test(Opts).

ct_groups(Module, Case) ->
    Group =
        try Module:groups() of
            Groups ->
                [ Name || {Name, _Props, Cases} <- Groups,
                          proplists:get_value(Case, Cases, false)]
        catch error:undef ->
                []
        end,
    case Group of
        [] ->
            [];
        _ ->
            [{group, Group}]
    end.

%%
%% Returns value of key with looking in the following order
%%
%% For a key called 'dir'
%%
%% - provided default value
%% - os env variable called EDT_DIR
%% - app env variable called dir
%%
-spec get_env(Key :: atom(), Default :: any()) -> any().
get_env(Key, Default) when is_atom(Key) ->
    Default1 = os:getenv(to_osenv_var(Key), Default),
    application:get_env(edt, Key, Default1).

get_env(Key) ->
    get_env(Key, undefined).

%% internal functions
to_osenv_var(Key) ->
    Key1 = atom_to_list(Key),
    "EDT_" ++  string:uppercase(Key1).
