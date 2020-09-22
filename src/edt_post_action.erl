-module(edt_post_action).

-behaviour(gen_server).

%% API
-export([start_link/0,
         start/0,
         stop/0]).

-export([add/2,
         delete/1,
         list/0]).

-export([event/1,
         sync_event/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {}).
-record(action,
        {name :: atom(),
         type = post :: 'post',
         func :: fun()}).

-define(TABLE, edt_post_action).

-type event() :: any().

%% Gen server control
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?SERVER).

%% Event Delivery
-spec event(Event :: event()) -> ok.
event(Event) ->
    gen_server:cast(?SERVER, {event, Event}).

-spec sync_event(Event :: event()) -> ok.
sync_event(Event) ->
    gen_server:call(?SERVER, {event, Event}).

%%
%% CRUD for actions
%%

%%
%% @doc
%%
%% Add a post action named `Name' to be run after edt_srv completes it's
%% actions. A post action can be a
%%
%% - A function ::: Run the function
%% - A tuple
%%   - {eunit, Module} ::: run eunit tests for the module
%%   - {ct, Module}    ::: run common tests for the module
%%
%% Many post actions are allowed and are uniquely identified by
%% `Name'. Post actions are run in the order of name, example
%%
%% For three post actions named z1, b1 and a1 are evaluated as
%% a1, b1,  z1
%%
%% All post actions are evaluated even if one of them raises an
%% error. To stop subsequent post actions from executing use
%% `throw(stop)' in the post action function
%%
%% @end
add(Name, {Type, Module}) when Type == eunit;
                               Type == ct ->
    Fun =
        fun() ->
                edt:test(Type, Module)
        end,
    add(Name, Fun);
add(Name, Fun) when is_atom(Name),
                    is_function(Fun) ->
    gen_server:call(?SERVER, {add, {Name, Fun}}).

delete(Name) when is_atom(Name) ->
    gen_server:call(?SERVER, {delete, Name}).

list() ->
    gen_server:call(?SERVER, list).

%% ---------------------------------------------------------
%% gen_server callbacks
%% ---------------------------------------------------------

init([]) ->
    ets:new(?TABLE, [named_table, {keypos, #action.name}, ordered_set]),
    {ok, #state{}}.

handle_call({add, {Name, Fun}}, _From, State) ->
    Action = #action{name = Name, func = Fun},
    Reply = ets:insert(?TABLE, Action),
    {reply, Reply, State};
handle_call({delete, Name}, _From, State) ->
    Reply = ets:delete(?TABLE, Name),
    {reply, Reply, State};
handle_call(list, _From, State) ->
    Actions = ets:tab2list(?TABLE),
    Reply = [Name || #action{name = Name} <- Actions],
    {reply, Reply, State};
handle_call({event, Event}, _From, State) ->
    Reply = actions(Event),
    {reply, Reply, State}.

handle_cast({event, Event}, State) ->
    actions(Event),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.

%% ---------------------------------------------------------
%% internal functions
%% ---------------------------------------------------------
actions(Event) ->
    Actions = ets:tab2list(?TABLE),
    Func =
        fun(Action, {stop, Acc}) ->
                #action{name=Name} = Action,
                Result = {error, {stop, Name}},
                {stop, Acc ++ [Result]};
           (Action, {_, Acc}) ->
                Result = maybe_do_action(Event, Action),
                StopOrCont =
                case Result of
                    {ok, {stop, _}} ->
                        stop;
                    _ ->
                        continue
                end,
                {StopOrCont, Acc ++ [Result]}
        end,
    {_, Result} = lists:foldl(Func, {start, []}, Actions),
    Result.

maybe_do_action(_Event, Action) ->
    #action{name=Name, func=Func} = Action,
    try
        Result = Func(),
        edt_out:stdout("action: ~p done result: ~p", [Name, Result]),
        {ok, {Name, Result}}
    catch
        throw:stop:_ ->
            {ok, {stop, Name}};
        C:E:_S ->
            edt_out:stdout("ERROR action ~p returned ~p:~p", [Name, C, E]),
            {error, {Name, {C, E}}}
    end.
