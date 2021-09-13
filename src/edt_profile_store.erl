%%
%% edt_profile_store is a in memory store for traces
%%
%% Copyright 2021 Raghav Karol.
%%
-module(edt_profile_store).

-export([
    crec_id/2,
    crec_id/4,
    find/1,
    find_crecs/1, find_crecs/2,
    find_srecs/0,
    init_store/0, init_store/1,
    srec/8,
    srec_id/5,
    store/1,
    track_call/7,
    track_summary/6,
    context_id/0
]).

-include_lib("stdlib/include/ms_transform.hrl").
-include("edt_profile.hrl").

%% ---------------------------------------------------------
%% Internal functions
%% ---------------------------------------------------------
table_name() ->
    edt_profile.

context_id() ->
    erlang:phash2(make_ref()).

crec_id(Pid, #call{} = S) ->
    #call{
        module = M,
        func = F,
        seq_no = SeqNo
    } = S,
    crec_id(Pid, M, F, SeqNo).

crec_id(Pid, M, F, SeqNo) ->
    erlang:phash2({Pid, M, F, SeqNo}).

srec_id(ContextId, Pid, M, F, Arity) ->
    erlang:phash2({Pid, ContextId, M, F, Arity}).

crec(ContextId, CallerId, Pid, Call, Result, EndTime, EndReds) ->
    Id = crec_id(Pid, Call),
    #crec{
        id = Id,
        context_id = ContextId,
        caller_id = CallerId,
        pid = Pid,
        module = Call#call.module,
        func = Call#call.func,
        arity = Call#call.arity,
        args = Call#call.args,
        result = Result,
        %TODO Don't use normal as default value
        return = normal,
        seq_no = Call#call.seq_no,
        start_time = Call#call.start_time,
        end_time = EndTime,
        start_reds = Call#call.start_reds,
        end_reds = EndReds
    }.

srec(ContextId, Pid, M, F, Arity, Count, Time, Reds) ->
    Id = srec_id(ContextId, Pid, M, F, Arity),
    #srec{
        id = Id,
        context_id = ContextId,
        module = M,
        func = F,
        arity = Arity,
        pid = Pid,
        count = Count,
        time = Time,
        reds = Reds
    }.

%% ---------------------------------------------------------
%% API
%% ---------------------------------------------------------
init_store() ->
    ets:new(table_name(), [public, named_table, {keypos, #rec.id}]).

init_store(delete_all) ->
    ets:delete_all_objects(table_name()).

store(#crec{id = Id} = Data) ->
    Rec = #rec{id = Id, data = Data},
    store(Rec);
store(#srec{id = Id} = Data) ->
    Rec = #rec{id = Id, data = Data},
    store(Rec);
store(#rec{} = Rec) ->
    ets:insert(table_name(), Rec).

find(Id) ->
    case ets:lookup(table_name(), Id) of
        [#rec{data = Data}] ->
            Data;
        [] ->
            {error, not_found}
    end.

find_crecs(CallerId) ->
    MS = ets:fun2ms(fun(#rec{data = #crec{caller_id = Id}} = A) when
        Id == CallerId
    ->
        A
    end),
    CRecs1 = [CRec || #rec{data = CRec} <- ets:select(edt_profile, MS)],
    lists:sort(fun(#crec{seq_no = A}, #crec{seq_no = B}) -> A =< B end, CRecs1).

find_crecs(Module, Func) ->
    MatchSpec = ets:fun2ms(
        fun(#rec{data = #crec{module = M1, func = F1}} = A) when
            M1 == Module, F1 == Func
        ->
            A
        end
    ),
    Recs = ets:select(table_name(), MatchSpec),
    [CRec || #rec{data = #crec{} = CRec} <- Recs].

find_srecs() ->
    MatchSpec = ets:fun2ms(fun(#rec{data = #srec{}} = A) -> A end),
    SRecs = [SRec || #rec{data = SRec} <- ets:select(edt_profile, MatchSpec)],
    lists:sort(
        fun(#srec{context_id = A}, #srec{context_id = B}) ->
            A =< B
        end,
        SRecs
    ).

track_call(ContextId, CallerId, Pid, Call, Result, EndTime, EndReds) ->
    CRec = crec(
        ContextId,
        CallerId,
        Pid,
        Call,
        Result,
        EndTime,
        EndReds
    ),
    store(CRec).

track_summary(ContextId, Pid, Call, Arity, EndTime, EndReds) ->
    Time = EndTime - Call#call.start_time,
    Reds = EndReds - Call#call.start_reds,
    Id = srec_id(ContextId, Pid, Call#call.module, Call#call.func, Arity),
    SRec1 =
        case find(Id) of
            #srec{} = SRec ->
                SRec#srec{
                    reds = SRec#srec.reds + Reds,
                    count = SRec#srec.count + 1,
                    time = SRec#srec.time + Time
                };
            {error, not_found} ->
                srec(ContextId, Pid, Call#call.module, Call#call.func, Arity, 1, Time, Reds)
        end,
    store(SRec1).
