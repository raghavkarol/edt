%%
%% Copyright 2021 Raghav Karol.
%%
%% TODO
%%
%% - Testing
%% - Use erlang trace instead of recon_trace
%% - Document limitation of the module for exceptions in recursive
%%   invocations of a function
%% - summary for exception calls as well
%%
%% - DONE
%%   Refactoring
%%
-module(edt_profile).

-behaviour(gen_server).

%% gen_server API
-export([
    start/0,
    start_link/0,
    stop/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

%% API
-export([
    trace/1,
    trace/2,
    trace/3,
    trace/4,
    trace_opts/2
]).

-export([stat_call/1, stat_call/2]).

-include_lib("stdlib/include/ms_transform.hrl").
-include("edt_profile.hrl").

-define(SERVER, ?MODULE).

-record(state, {
    context_stack,
    call_stack,
    seq_no,
    opts
}).

%% ---------------------------------------------------------
%% Gen server API
%% ---------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?SERVER).

%% ---------------------------------------------------------
%% API
%% ---------------------------------------------------------
%% link @{trace_opt/2}
trace(M) when is_atom(M) ->
    trace(M, '_');
trace(Specs) when is_list(Specs) ->
    TOpts = default_opts(topts),
    trace_opts(Specs, TOpts).

trace(M, F) ->
    Specs = [to_trace_spec({M, F})],
    trace(Specs).

trace(M, F, FOpts) ->
    Specs = [to_trace_spec({M, F, FOpts})],
    trace(Specs).

%% link @{trace_opt/2}
trace(M, F, ArgSpec, FOpts) ->
    Specs = [to_trace_spec({M, F, ArgSpec, FOpts})],
    trace(Specs).

%%
%% Specs is a list of
%%
%% [Module|{Module, Func}|{Module, Func, FOpts}|{Module, Func, FOpts, ArgSpec}]
%%
%% Opts is map with the following keys
%%
%% track_calls :: tracks information to build a callgraph when set to true
%% max_calls   :: number of calls to record
%%
%% NOTE: track_calls should be used with caution in production as it
%% will make an entry for all traced calls to be able to recreate a
%% call graph
%%
%% FOpts is a map with the following keys
%%
%% capture_args   :: Capture the arguments for the function call
%% capture_result :: Capture the result for the function call
%% capture       :: Capture both args and result
%% start_context :: Start a new context for the function call, all
%%
%% child calls from the function starting a context are tracked and
%% can be related to the parent.
%%
trace_opts(Specs, TOpts) ->
    TOpts1 = maps:merge(default_opts(topts), TOpts),
    Specs1 = lists:map(fun to_trace_spec/1, Specs),
    gen_server:call(?SERVER, {trace, Specs1, TOpts1}).

%% @doc describe a specific call by id
stat_call(Id) ->
    edt_profile_store:find(Id).

%% @doc describe all calls for module and function
stat_call(Module, Func) ->
    Calls = edt_profile_store:find_crecs(Module, Func),
    lists:sort(fun(#crec{seq_no = A}, #crec{seq_no = B}) -> A =< B end, Calls).

%% ---------------------------------------------------------
%% Gen server callbacks
%% ---------------------------------------------------------
init([]) ->
    edt_profile_store:init_store(),
    {ok, new_state()}.

handle_call({trace, Specs, TOpts}, _From, _State) ->
    edt_profile_store:init_store(delete_all),
    {Reply, Opts} = trace1(Specs, TOpts),
    State1 = new_state(),
    State2 = State1#state{opts = Opts},
    {reply, Reply, State2}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({call, {Pid, M, F, Args, Arity, StartTime, StartReds}}, State) ->
    #state{seq_no = SeqNo, call_stack = Stack} = State,
    {ContextId, State1} = maybe_start_context(Pid, M, F, State),
    Call = new_call(M, F, Args, Arity, SeqNo, StartTime, StartReds),
    Stack1 = push(Stack, Pid, Call),
    maybe_track_call(ContextId, Pid, Call, State),
    State2 = State1#state{seq_no = SeqNo + 1, call_stack = Stack1},
    {noreply, State2};
handle_info({return_from, {Pid, M, F, Arity, Result, EndTime, EndReds}}, State) ->
    %% io:format("~s:~p: handle_info return_from: ~p:~p ~n", [string:replace(code:which(?MODULE), ".beam", ".erl"), ?LINE, M, F]),
    {ContextId, State1} = maybe_stop_context(Pid, M, F, State),
    {Call, ExCalls, Stack1} = pop(Pid, M, F, State),
    State2 = State1#state{call_stack = Stack1},
    %TODO track return values and times
    maybe_track_call(ContextId, Pid, Call, State2, Result, EndTime, EndReds),
    maybe_track_exceptions(State, Pid, ExCalls),
    %% TODO: summary for exception calls as well?

    % Store aggregate results accros pids
    edt_profile_store:track_summary(ContextId, Pid, Call, Arity, EndTime, EndReds),
    {noreply, State2}.

terminate(_Reason, _State) ->
    ok.
%% ---------------------------------------------------------
%% Internal functions
%% ---------------------------------------------------------
default_opts(topts) ->
    Track = edt:get_env(profile_track_calls, false),
    Max = edt:get_env(profile_max_calls, 100),
    #{
        track_calls => Track,
        max_calls => Max
    };
default_opts(fopts) ->
    Capture = edt:get_env(profile_capture, false),
    StartContext = edt:get_env(profile_start_context, false),
    #{
        capture => Capture,
        start_context => StartContext
    }.

new_state() ->
    #state{
        context_stack = #{},
        call_stack = new_call_stack(),
        seq_no = 0,
        opts = #{}
    }.

timestamp() ->
    erlang:system_time(microsecond).

new_call_stack() ->
    #{}.

new_call(M, F, Args, Arity, SeqNo, StartTime, StartReds) ->
    #call{
        module = M,
        func = F,
        args = Args,
        arity = Arity,
        seq_no = SeqNo,
        start_time = StartTime,
        start_reds = StartReds
    }.

push(Stack, Pid, Top) ->
    maps:update_with(Pid, fun(Stack1) -> [Top | Stack1] end, [Top], Stack).

pop(Pid, Stack) ->
    Top = peek(Pid, Stack, undefined),
    Stack1 = maps:update_with(
        Pid,
        fun
            ([_ | Stack1]) ->
                Stack1;
            ([]) ->
                []
        end,
        [],
        Stack
    ),
    {Top, Stack1}.

peek(Pid, Stack, Default) ->
    case maps:get(Pid, Stack, []) of
        [Top | _] ->
            Top;
        [] ->
            Default
    end.

all(Pid, Stack) ->
    maps:get(Pid, Stack, []).

%% Partitions the calls into two lists
%%  List1 contains all consecutive calls what do not match M and F
%%  List2 is the remaining calls that start with M and F
%%
%% Example:
%%  partition(b, f1, [{a, f1}, {a, f2}, {b, f1}, {c, f1}, ...])
%%  returns:
%%   {[{a, f1}, {a, f2}], [{b, f1}, {c, f1}, ...]}
partition(M, F, Calls) ->
    Pred =
        fun(#call{module = M1, func = F1}) ->
            not (M1 == M andalso F1 == F)
        end,
    {lists:takewhile(Pred, Calls), lists:dropwhile(Pred, Calls)}.

pop(Pid, M, F, State) ->
    #state{call_stack = Stack} = State,
    case all(Pid, Stack) of
        [] ->
            {undefined, [], Stack};
        Calls ->
            %TODO include arity
            {ExCalls, Calls2} = partition(M, F, Calls),
            Call = hd(Calls2),
            Calls3 =
                case Calls2 of
                    [] ->
                        [];
                    _ ->
                        lists:nthtail(1, Calls2)
                end,
            {Call, ExCalls, Stack#{Pid => Calls3}}
    end.

track_exception(Pid, Call) ->
    Id = edt_profile_store:crec_id(Pid, Call),
    CRec = edt_profile_store:find(Id),
    edt_profile_store:store(CRec#crec{return = exception}).

start_context1(State, Pid) ->
    #state{context_stack = Ids} = State,
    Id = edt_profile_store:context_id(),
    Ids1 = push(Ids, Pid, Id),
    State1 = State#state{context_stack = Ids1},
    {Id, State1}.

stop_context1(State, Pid) ->
    #state{context_stack = Ids} = State,
    {Id, Ids1} = pop(Pid, Ids),
    {Id, State#state{context_stack = Ids1}}.

maybe_track_exceptions(#state{opts = #{track_calls := true}}, Pid, ExCalls) ->
    Fun =
        fun(ECall) ->
            track_exception(Pid, ECall)
        end,
    lists:foreach(Fun, ExCalls);
maybe_track_exceptions(_, _, _) ->
    ok.

maybe_track_call(ContextId, Pid, Call, State) ->
    Result = '$none',
    EndTime = 0,
    EndReds = 0,
    maybe_track_call(ContextId, Pid, Call, State, Result, EndTime, EndReds).

maybe_track_call(_ContextId, _Pid, undefined, _State, _Result, _EndTime, _EndReds) ->
    ok;
maybe_track_call(ContextId, Pid, Call, State, Result, EndTime, EndReds) ->
    #state{
        call_stack = Stack,
        opts = Opts
    } = State,
    case Opts of
        #{track_calls := true} ->
            case maps:get(Pid, Stack, []) of
                [PCall | _] ->
                    CallerId = edt_profile_store:crec_id(Pid, PCall);
                [] ->
                    CallerId = 0
            end,
            edt_profile_store:track_call(ContextId, CallerId, Pid, Call, Result, EndTime, EndReds);
        _ ->
            ok
    end.

maybe_start_context(Pid, M, F, State) ->
    #state{opts = Opts} = State,
    FOpts = maps:get({M, F}, Opts, []),
    {ContextId, State1} =
        case FOpts of
            #{start_context := true} ->
                start_context1(State, Pid);
            _ ->
                {'$none', State}
        end,
    {ContextId, State1}.

maybe_stop_context(Pid, M, F, State) ->
    #state{opts = Opts} = State,
    FOpts = maps:get({M, F}, Opts, []),
    {ContextId, State1} =
        case FOpts of
            #{start_context := true} ->
                stop_context1(State, Pid);
            _ ->
                {'$none', State}
        end,
    {ContextId, State1}.

to_trace_spec(M) when is_atom(M) ->
    to_trace_spec({M, '_'});
to_trace_spec({M, F}) ->
    FOpts = default_opts(fopts),
    to_trace_spec({M, F, '_', FOpts});
to_trace_spec({M, F, FOpts}) ->
    FOpts1 = maps:merge(default_opts(fopts), FOpts),
    to_trace_spec({M, F, FOpts1, '_'});
to_trace_spec({_, _, _, _} = Spec) ->
    Spec.

reductions(Pid) ->
    case erlang:process_info(Pid) of
        undefined ->
            0;
        Info ->
            proplists:get_value(reductions, Info)
    end.

maybe_capture_args(#{capture := true}, Args) ->
    Args;
maybe_capture_args(#{capture_args := true}, Args) ->
    Args;
maybe_capture_args(_, _) ->
    '$not_captured'.

maybe_capture_result(#{capture := true}, Result) ->
    Result;
maybe_capture_result(#{capture_result := true}, Result) ->
    Result;
maybe_capture_result(_, _) ->
    '$not_captured'.

fopts(M, F, Opts) ->
    Default = default_opts(fopts),
    Map1 = maps:get({M, '_'}, Opts, Default),
    maps:get({M, F}, Opts, Map1).

fun_capture_args(Opts, Caller) ->
    fun
        ({trace, Pid, call, {M, F, Args}}) ->
            Reductions = reductions(Pid),
            FOpts = fopts(M, F, Opts),
            Arity = length(Args),
            Args1 = maybe_capture_args(FOpts, Args),
            Caller ! {call, {Pid, M, F, Args1, Arity, timestamp(), Reductions}},
            "";
        ({trace, Pid, return_from, {M, F, Arity}, Result}) ->
            Reductions = reductions(Pid),
            FOpts = fopts(M, F, Opts),
            Result1 = maybe_capture_result(FOpts, Result),
            Caller ! {return_from, {Pid, M, F, Arity, Result1, timestamp(), Reductions}},
            ""
    end.

init_recon_opts(Opts) ->
    Fun = fun_capture_args(Opts, self()),
    Pid = maps:get(pid, Opts, all),
    [
        {pid, Pid},
        {scope, local},
        {formatter, Fun}
    ].

init_tracer(Specs, Opts) ->
    Specs1 = lists:map(
        fun({M, F, ArgSpec, _FOpts}) ->
            {M, F, [{ArgSpec, [], [{return_trace}]}]}
        end,
        Specs
    ),
    ReconOpts = init_recon_opts(Opts),
    MaxCalls = maps:get(max_calls, Opts, 10),
    ok = recon_trace:clear(),
    recon_trace:calls(Specs1, MaxCalls, ReconOpts).

trace1(Specs, TOpts) ->
    Opts1 = lists:foldl(
        fun({M, F, _ArgSpec, FOpts}, Acc) ->
            Acc#{{M, F} => FOpts}
        end,
        #{},
        Specs
    ),

    Opts2 = maps:merge(TOpts, Opts1),
    Result = init_tracer(Specs, Opts2),
    {Result, Opts2}.
