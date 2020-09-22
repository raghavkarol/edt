%%%-------------------------------------------------------------------
%% @doc edt public API
%% @end
%%%-------------------------------------------------------------------

-module(edt_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Cmd = os:cmd("ps "++ os:getpid()),
    Profile = edt:rebar3_profile(),
    case parse_rebar3_profile(Cmd) of
        {error, Reason} ->
            edt_out:stdout("Failed to determine rebar3 profile we are running as reason: ~p ", [Reason]);
        Profile ->
            ok;
        Profile1 ->
            edt_out:stdout("--------------------------------------------------------------------------------"),
            edt_out:stdout(),
            edt_out:stdout("WARNING configured with rebar3 profile: ~s but running with ~s ", [Profile, Profile1]),
            edt_out:stdout(),
            edt_out:stdout("~s", [Cmd]),
            edt_out:stdout(),
            edt_out:stdout("--------------------------------------------------------------------------------", [])
    end,
    init_cowboy(),
    edt_sup:start_link().

stop(_State) ->
    cowboy:stop_listener(edt_http_listener),
    ok.

parse_rebar3_profile(Cmd) ->
    case re:run(Cmd, "rebar3\s+shell|as\s+(.+?)\s+shell", [{capture, all, list}]) of
        {match, [_, Profile]} ->
            Profile;
        {match, [_]} ->
            "default";
        Result ->
            {error, {not_parseable, {Result, Cmd}}}
    end.

init_cowboy() ->
    Dispatch = cowboy_router:compile(
                 [{'_', [{"/flycheck", edt_http, #{}}]}]),
    Port = edt:http_port(),
    {ok, _} = cowboy:start_clear(
                edt_http_listener,
                [{port, Port}],
                #{env => #{dispatch => Dispatch}}).
