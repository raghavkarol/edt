%%
%% This module monitors the group leader for the erlang node edt runs
%% in and sends all output to that group leader.
%%
%% When calling edt functions from a distributed erlang node .e.g.,
%% distel, we want to send all output from edt to the node where edt
%% is running and not the remote node.
%%
%%
%% Copyright 2020 Raghav Karol.
%%
-module(edt_out).

-behaviour(gen_server).

%% API
-export([
    start/0,
    start_link/0,
    stop/0
]).

-export([
    io_server/0,
    stdout/1,
    stdout/2,
    stdout/3
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2
]).

-define(SERVER, ?MODULE).

-record(state, {
    ref :: reference(),
    io_server :: pid()
}).

%% ---------------------------------------------------------
%% Gen server control API
%% ---------------------------------------------------------
start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?SERVER).

%% ---------------------------------------------------------
%% API
%% ---------------------------------------------------------
io_server() ->
    case erlang:whereis(?SERVER) of
        undefined ->
            group_leader();
        _ ->
            gen_server:call(?SERVER, io_server)
    end.

stdout(Msg) ->
    stdout(Msg, []).

stdout(Fmt, Args) ->
    stdout(Fmt, Args, nl).

stdout(Fmt, Args, no_nl) ->
    io:format(io_server(), "==> ~s ", [datetime()]),
    io:format(io_server(), Fmt, Args);
stdout(Fmt, Args, nl) ->
    stdout(Fmt, Args, no_nl),
    io:format(io_server(), "~n", []).

%% ---------------------------------------------------------
%% Gen server callbacks
%% ---------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    GL = group_leader(),
    {ok, #state{io_server = GL}}.

handle_call(io_server, _From, State) ->
    #state{io_server = Pid} = State,
    {reply, Pid, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

%% ---------------------------------------------------------
%% Internal functions
%% ---------------------------------------------------------
datetime() ->
    Pad =
        fun(V) ->
            io_lib:format("~2..0B", [V])
        end,
    {{Year, Mon, Day}, {Hour, Min, Sec}} = calendar:local_time(),
    Str =
        fun(V) ->
            edt_lib:to_string(V)
        end,

    DT = [
        Str(Year),
        $-,
        Str(Mon),
        $-,
        Str(Day),
        " ",
        Pad(Hour),
        $:,
        Pad(Min),
        $:,
        Pad(Sec)
    ],
    iolist_to_binary(DT).
