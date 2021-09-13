-ifndef(_edt_profile_h).
-define(_edt_profile_h, 1).

%% @doc rec is a super type for all records
-record(rec, {id, data}).

%% @doc crec is a function call record
-record(crec, {
    id,
    context_id,
    caller_id,
    pid,
    module,
    func,
    args,
    arity,
    result,
    seq_no,
    return,
    start_time,
    end_time,
    start_reds,
    end_reds
}).

%% @srec is a summary record
-record(srec, {
    id,
    context_id,
    module,
    func,
    arity,
    pid,
    count,
    time,
    reds
}).

%% @call records a function call on the stack
-record(call, {
    module,
    func,
    args,
    arity,
    seq_no,
    start_time,
    start_reds
}).

-endif.
