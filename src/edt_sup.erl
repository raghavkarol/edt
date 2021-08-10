%%
%% Top Level supervisor
%%
%% Copyright 2020 Raghav Karol.
%%
-module(edt_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all, intensity => 60, period => 60},

    ChildSpecs = [#{id => edt_srv,
                    start => {edt_srv, start_link, [edt:home()]}},
                  #{id => edt_post_action,
                    start => {edt_post_action, start_link, []}},
                  #{id => edt_out,
                    start => {edt_out, start_link, []}},
                  #{id => edt_profile,
                    start => {edt_profile, start_link, []}}],
    {ok, {SupFlags, ChildSpecs}}.


%% internal functions
