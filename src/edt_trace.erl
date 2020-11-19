-module(edt_trace).

-behaviour(gen_server).

%% API
-export([start_link/0,
         start/0,
         stop/0]).

-export([trace/2,
         trace/3,
         trace_result/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-define(SERVER, ?MODULE).

%% ---------------------------------------------------------
%% API
%% ---------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?SERVER).

%%
%% @doc
%%
%% Trace M:F(...) and store the arguments and return value so that they can be retrieved
%%
%% @end
%%
trace(M, F) ->
    gen_server:call(?SERVER, {trace, {M, F, '_'}}).

trace(M, F, ArgsSpec) ->
    gen_server:call(?SERVER, {trace, {M, F, ArgsSpec}}).

trace_result(M, F) ->
    gen_server:call(?SERVER, {trace_result, {M, F}}).

%% ---------------------------------------------------------
%% Gen server callbacks
%% ---------------------------------------------------------
init([]) ->
    {ok, #{}}.

handle_call({trace, {M, F, ArgsSpec}}, _From, State) ->
    Reply = trace1(M, F, ArgsSpec),
    {reply, Reply, State};
handle_call({trace_result, {M, F}}, _From, State) ->
    Fun = fun({M1, F1, _}, _) ->
                  {M, F} == {M1, F1}
          end,
    Result1 = maps:filter(Fun, State),
    Reply = latest_call(Result1),
    {reply, Reply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({call, {{M, F, Pid}, Args}}, State) ->
    Now = erlang:system_time(seconds),
    Key = {M, F, Pid},
    Value = #{start_ts => Now,
              args => Args},
    Fun = fun(_) ->
                  Value
          end,
    State1 = maps:update_with(Key, Fun, Value, State),
    {noreply, State1};
handle_info({return_from, {{M, F, Pid}, Result}}, State) ->
    Now = erlang:system_time(seconds),
    Key = {M, F, Pid},
    Fun = fun(Value) ->
                  Value#{end_ts => Now,
                         result => Result}
          end,
    State1 = maps:update_with(Key, Fun, State),
    {noreply, State1}.

terminate(_Reason, _State) ->
    ok.

%% ---------------------------------------------------------
%% Internal functions
%% ---------------------------------------------------------
trace1(M, F, ArgsSpec) ->
    Fun = fun_capture_args(self()),
    Spec = {M, F, [{ArgsSpec, [], [{return_trace}]}]},
    Opts = [{scope, local},
            {formatter, Fun}],
    recon_trace:calls(Spec, 10, Opts).

fun_capture_args(Caller) ->
    fun({trace, Pid, call, {M, F, Args}}) ->
            Caller ! {call, {{M, F, Pid}, Args}},
            ok;
       ({trace, Pid, return_from, {M, F, _Arity}, Result}) ->
            Caller ! {return_from, {{M, F, Pid}, Result}},
            ok
    end.

latest_call(Result) when map_size(Result) == 0 ->
    not_found;
latest_call(Result) ->
    Result1 = maps:values(Result),
    Fun = fun(#{end_ts := E1}, #{end_ts := E2}) ->
                  E2 =< E1
          end,
    [Result2|_] = lists:sort(Fun, Result1),
    maps:with([args, result], Result2).
