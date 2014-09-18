
-module(ecache_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Type, I), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD(Type, I, Args),  {I,  {I, start_link, [Args]}, permanent, 5000, Type, [I]}).

-include("internal.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    SettingKeys = [{ets_threshold, undefined}, {ets_maxsize, 512*1024*1024}, {access_weight, 30}],
    Args = [{Key, ecache:get_env(Key, Default)}  || {Key, Default} <- SettingKeys],
    ?info("app_env:~p", [Args]),
    {ok, { {one_for_one, 5, 10}, [?CHILD(worker, ecache_server, Args)]}}.

