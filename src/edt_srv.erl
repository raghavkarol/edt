%%
%%
%%
-module(edt_srv).

-behaviour(gen_statem).

%% API
-export([start/1,
         start_link/1,
         stop/0]).

-export([changes/0,
         process_changes/0]).

-export([test/1]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([handle_event/4]).

-define(SERVER, ?MODULE).
-define(CHANGES, edt_srv_changes).

-record(data, {}).

-include("edt_srv.hrl").

%% -------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------
start(Dir) ->
    gen_statem:start({local, ?SERVER}, ?MODULE, [Dir], []).

start_link(Dir) ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [Dir], []).

stop() ->
    gen_statem:stop(?SERVER).

changes() ->
    gen_statem:call(?SERVER, changes).

process_changes() ->
    gen_statem:call(?SERVER, process_changes).

test({Type, Path})
  when Type == eunit;
       Type == ct ->
    gen_statem:call(?SERVER, {test, {Type, Path}}).

%% -------------------------------------------------------------------
%% gen_statem callbacks
%% -------------------------------------------------------------------
callback_mode() ->
    handle_event_function.

init([Dir]) ->
    ets:new(?CHANGES, [named_table, {keypos, #change.path}]),
    fs:start_link(watch, Dir),
    fs:subscribe(watch),
    {ok, idle, #data{}}.

handle_event({call, From}, changes, _, _) ->
    Reply = lists:sort(changes1()),
    {keep_state_and_data, [{reply, From, Reply}]};
handle_event({call, From}, {test, {Type, Path}}, _State, Data) ->
    Path1 = filename:absname(Path),
    Result = edt:compile(Path1, [{d, 'TEST'}]),
    case Result of
        {ok, {Path1, Module, Warnings}} ->
            edt_out:stdout("~s", [edt_lib:format(Warnings, warning)], no_nl),
            edt:ensure_code_path(Path1),
            edt:test(Type, Module),
            Reply = ok;
        {error, {Path1, Errors, Warnings}} ->
            edt_out:stdout("~s", [edt_lib:format(Errors, error)], no_nl),
            edt_out:stdout("~s", [edt_lib:format(Warnings, warning)], no_nl),
            Reply = {error, compilation_failed}
    end,
    {next_state, changes, Data, [{reply, From, Reply}]};
handle_event({call, From}, process_changes, idle, _) ->
    Reply = [],
    {keep_state_and_data, [{reply, From, Reply}]};
handle_event({call, From}, process_changes, changes, Data) ->
    Reply = handle_process_changes_event(),
    {next_state, idle, Data, [{reply, From, Reply}]};
handle_event(state_timeout, process_changes, changes, Data) ->
    handle_process_changes_event(),
    {next_state, idle, Data};
handle_event(info, Msg, _State, Data) ->
    {_Pid, {fs, file_event}, {Path, Flags}} = Msg,
    Ignore = ignore_path(edt:ignore_regex(), Path),
    AutoProcess = edt:auto_process(),
    case Ignore of
        true ->
            logger:notice("Ignoring Path:~p IgnoreRegex: ~p", [Path, edt:ignore_regex()]);
        false ->
            PathType = edt:file_type(Path),
            logger:debug("Path:~p Flags: ~p ~n", [Path, Flags]),
            handle_info_event(PathType, Path, Flags)
    end,
    Events =
        case {Ignore, AutoProcess} of
            {false, true} ->
                [{state_timeout, 500, process_changes}];
            _ ->
                []
        end,
    {next_state, changes, Data, Events}.

terminate(_Reason, _State, _Data) ->
    void.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------
handle_info_event(src, Path, Flags) ->
    Modified = lists:member(modified, Flags),
    Removed = lists:member(removed, Flags),
    case {Modified, Removed} of
        {_, true} ->
            ets:delete(?CHANGES, Path),
            ok;
        {true, false} ->
            ets:insert_new(?CHANGES, #change{path=Path, action=compile}),
            ets:update_counter(?CHANGES, Path, {#change.count, 1}),
            ok;
        _ ->
            ok
    end;
handle_info_event(PathType, Path, _Flags) ->
    logger:debug("Don't know how to handle Path: ~p with filetype ", [Path, PathType]).

handle_process_changes_event() ->
    Changes = changes1(),
    ets:delete_all_objects(?CHANGES),
    Fun = fun_compile(),
    lists:foreach(Fun, Changes),
    Changes.

ignore_path(<<"">>, _) ->
    false;
ignore_path(IgnoreRegex, Path) ->
    match == re:run(Path, IgnoreRegex, [{capture, none}]).

changes1() ->
    ets:tab2list(?CHANGES).

fun_compile() ->
    fun(#change{action=compile, path = Path}) ->
            Result = edt:compile(Path, []),
            Report = edt_lib:report(Result),
            edt_out:stdout("~s", [Report]),
            maybe_do_post_actions(Result)
    end.

%% TODO: define a type for edt:compile() - result
maybe_do_post_actions({ok, {Path, _, _}}) ->
    edt_post_action:event({compile, Path});
maybe_do_post_actions(_) ->
    ok.
