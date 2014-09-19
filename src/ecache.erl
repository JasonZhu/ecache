-module(ecache).

%% API
-export([get/1, set/2, set/3, delete/1]).

%% APP
-export([start/0, stop/0, get_env/1, get_env/2]).

-define(APP, ?MODULE).
-include("internal.hrl").
%% ===================================================================
%% App
%% ===================================================================
start() ->
    start(?APP).

stop() ->
    application:stop(?APP).

get_env(Key) ->
    get_env(Key, undefined).

get_env(Key, Default) ->
    case application:get_env(?APP, Key) of
        undefined ->
            Default;
        {ok, Value} ->
            Value
    end. 

%% ===================================================================
%% API
%% ===================================================================
-spec get(key()) -> maybe().
get(Key) ->
    ecache_server:get(Key).

-spec set(key(), val()) -> ok.
set(Key, Val) ->
    set(Key, Val, ?DEFAULT_TTL).

-spec set(key(), val(), integer()) -> ok.
set(Key, Val, TTL) ->
    ecache_server:set(Key, Val, TTL).

-spec delete(key()) -> ok.
delete(Key) ->
    ecache_server:delete(Key).

%% ===================================================================
%% Helper
%% ===================================================================
start(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok;
        {error, {not_started, Dep}} ->
            ok = start(Dep),
            start(App)
    end.