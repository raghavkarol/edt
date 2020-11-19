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
    case edt_lib:parse_rebar3_profile(Cmd) of
        Profile ->
            ok;
        Other ->
            case application:get_env(edt, warn_rebar3_profile, true) of
                true ->
                    edt_out:stdout("--------------------------------------------------------------------------------"),
                    edt_out:stdout("WARNING configured with rebar3 profile: ~s but running with ~p ", [Profile, Other]),
                    edt_out:stdout("~s", [Cmd], no_nl),
                    edt_out:stdout("--------------------------------------------------------------------------------", []);
                false ->
                    ok
            end
    end,
    init_cowboy(edt:enable_http_server()),
    edt_sup:start_link().

stop(_State) ->
    ok.

init_cowboy(Enable)
  when Enable == true ->
    Dispatch = cowboy_router:compile(
                 [{'_', [{"/compile", edt_http, #{}}]}]),
    Port = edt:http_port(),
    {ok, _} = cowboy:start_clear(
                edt_http_listener,
                [{port, Port}],
                #{env => #{dispatch => Dispatch}});
init_cowboy(_) ->
    ok.
