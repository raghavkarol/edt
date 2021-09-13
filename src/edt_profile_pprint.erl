%%
%% edt_profile_pprint is a pretty printer for profiles created by
%% edt_profile.
%%
%% Copyright 2021 Raghav Karol.
%%
-module(edt_profile_pprint).

-include("edt_profile.hrl").

-export([
    summary/0, summary/1,
    call_graph/0, call_graph/1
]).

%% ---------------------------------------------------------
%% Internal functions
%% ---------------------------------------------------------
indent(I) ->
    string:join(lists:duplicate(I bsl 1, " "), "").

to_string1({mfa, {M, F, A}}) ->
    io_lib:format("~p:~p/~p", [M, F, A]);
to_string1(Term) when is_list(Term) ->
    io_lib:format("~s", [Term]);
to_string1(Term) ->
    io_lib:format("~p", [Term]).

to_string({Width, Term}) ->
    S = to_string1(Term),
    Fmt = lists:flatten([$~, to_string1(Width), $s]),
    lists:flatten(io_lib:format(Fmt, [S]));
to_string(Term) when is_list(Term) ->
    Term;
to_string(Term) ->
    io_lib:format("~s", [to_string1(Term)]).

line(Tokens) ->
    Tokens1 = lists:map(fun to_string/1, Tokens),
    string:join(Tokens1, " ").

to_proplist(#crec{} = CRec) ->
    #crec{
        id = Id,
        pid = Pid,
        return = Return,
        seq_no = SeqNo,
        start_time = ST,
        end_time = ET,
        start_reds = SR,
        end_reds = ER
    } = CRec,
    [
        {id, Id},
        {pid, Pid},
        {time,
            case ET of
                0 -> "-";
                _ -> ET - ST
            end},
        {reductions,
            case ER of
                0 -> "-";
                _ -> ER - SR
            end},
        {return, Return},
        {seq_num, SeqNo}
    ];
to_proplist(#srec{} = SRec) ->
    #srec{
        context_id = ContextId,
        pid = Pid,
        count = C,
        time = Time,
        reds = Reds
    } = SRec,
    [
        {context_id, ContextId},
        {pid, Pid},
        {count, C},
        {time, Time},
        {reds, Reds}
    ].

header(Fields, #srec{}) ->
    Spec = [
        {context_id, {10, "context_id"}},
        {pid, {15, "pid"}},
        {count, {10, "count"}},
        {time, {10, "time"}},
        {reductions, {15, "reductions"}}
    ],
    case Fields of
        all ->
            [S || {_, S} <- Spec];
        _ ->
            [proplists:get_value(Field, Spec) || Field <- Fields]
    end;
header(Fields, #crec{}) ->
    Spec = [
        {id, {10, "id"}},
        {pid, {15, "pid"}},
        {time, {10, "time"}},
        {reductions, {15, "reductions"}},
        {return, {10, "return"}},
        {seq_num, {10, "seq_num"}}
    ],
    case Fields of
        all ->
            [S || {_, S} <- Spec];
        _ ->
            [proplists:get_value(Field, Spec) || Field <- Fields]
    end.

select(Fields, Data) ->
    PL = to_proplist(Data),
    Fields1 =
        case Fields of
            all ->
                [Field || {Field, _} <- PL];
            _ ->
                Fields
        end,
    [proplists:get_value(Field, PL) || Field <- Fields1].

to_iodata(Fields, Header, #srec{} = SRec) ->
    #srec{
        module = M,
        func = F,
        arity = Arity
    } = SRec,
    Values = select(Fields, SRec),
    Widths = [W || {W, _} <- Header],
    lists:zip(Widths, Values) ++
        [{-40, {mfa, {M, F, Arity}}}];
to_iodata(Fields, Header, {#crec{} = CRec, Indent}) ->
    #crec{
        module = M,
        func = F,
        arity = Arity
    } = CRec,
    Values = select(Fields, CRec),
    Widths = [W || {W, _} <- Header],
    lists:zip(Widths, Values) ++
        [
            indent(Indent),
            {-40, {mfa, {M, F, Arity}}}
        ].

call_graph(Fields, CallerId, Indent) ->
    CRecs = edt_profile_store:find_crecs(CallerId),
    Header = header(Fields, #crec{}),
    [
        begin
            Call = iolist_to_binary([line(to_iodata(Fields, Header, {CRec, Indent})), "\n"]),
            io:format("~s", [Call]),
            call_graph(Fields, CRec#crec.id, Indent + 1)
        end
     || CRec <- CRecs
    ],
    ok.

%% ---------------------------------------------------------
%% API
%% ---------------------------------------------------------

%% @doc call_graph
call_graph(Fields) ->
    Header = header(Fields, #crec{}),
    io:format("~s ~s ~n~n", [line(Header), "MFA"]),
    call_graph(Fields, 0, 0).

call_graph() ->
    call_graph(all).

%% @doc summary
summary(Fields) ->
    SRecs = edt_profile_store:find_srecs(),
    Header = header(Fields, #srec{}),
    Summary = [[line(to_iodata(Fields, Header, SRec)), "\n"] || SRec <- SRecs],
    Summary1 = iolist_to_binary(Summary),
    io:format("~s ~s ~n~n~s", [line(Header), to_string({-40, "MFA"}), Summary1]).

summary() ->
    summary(all).