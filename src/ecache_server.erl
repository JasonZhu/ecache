-module(ecache_server).

-behaviour(gen_server).

%% API
-export([
    start_link/0, 
    start_link/1,
    get/1, 
    set/3, 
    delete/1]).

%% gen_server callbacks
-export([
    init/1, 
    handle_call/3, 
    handle_cast/2,
    handle_info/2, 
    terminate/2, 
    code_change/3]).

-include("internal.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-record(state, {ets_threshold, ets_maxsize, ets_name, access_weight}).
-record(cache,{key, val, start_time, ttl, access_cnt=0}).

%% ===================================================================
%% API
%% ===================================================================

start_link() ->
    start_link([]).

start_link(Arg) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Arg, []).

get(Key) ->
    ?debug("[GET] Key: ~p", [Key]),
    gen_server:call(?MODULE, {get, Key}).

set(Key, Value, TTL) ->
    ?debug("[SET] Key: ~p, Val: ~p TTL: ~p", [Key, Value, TTL]),
    gen_server:cast(?MODULE, {set, Key, Value, TTL}).

delete(Key) ->
    ?debug("[DELETE] Key: ~p", [Key]),
    gen_server:cast(?MODULE, {delete, Key}).

%% ===================================================================
%% gen_server callback
%% ===================================================================
init(Opts) ->
    ?info("init ecache server with opts:~p", [Opts]),
    MaxSize = proplists:get_value(ets_maxsize, Opts),
    Threshold = proplists:get_value(ets_threshold, Opts),
    Weight = proplists:get_value(access_weight, Opts),
    ETSName = ets:new(ecache_server, [{keypos, #cache.key}, private]),
    {ok, #state{
        ets_maxsize = MaxSize,
        ets_threshold = Threshold,
        ets_name = ETSName,
        access_weight = Weight}}.

handle_call({get, Key}, _From, #state{ 
    ets_name = ETSName} = State) ->
    Reply= lookup(ETSName, Key),
    {reply, Reply, State};
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast({set, Key, Value, TTL}, #state{
        ets_maxsize = MaxSize, 
        ets_threshold = Threshold,
        ets_name = ETSName,
        access_weight = Weight} = State) ->

    ets:insert(ETSName, #cache{
                        key= Key, 
                        val=Value, 
                        start_time=current_time(), 
                        ttl=TTL}),

    case check_memsize(ETSName, MaxSize) of
        over ->

            N = delete_expired(ETSName),
            ?info("delete num of expired items:~p", [N]),
            delete_light_items(ETSName, Threshold*MaxSize, Weight);
        _ ->
            ok
    end,
    {noreply, State};
handle_cast({delete, Key}, #state{
        ets_name = ETSName} = State) ->
    ets:delete(ETSName, Key),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, #state{ets_name = ETSName}) ->
    ?debug("cache terminates"),
    ets:delete(ETSName),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

current_time()->
    Now = calendar:local_time(),
    calendar:datetime_to_gregorian_seconds(Now).

check_memsize(ETSName, Size) ->
    F = fun (X) ->
        case X of 
            {memory, _} -> true;
            _ -> false
        end
    end,
    [{_, H}|_] = lists:filter(F, ets:info(ETSName)),
    ValueSize = H * erlang:system_info(wordsize),
    if
        ValueSize > Size -> 
            ?warning("ValueSize ~p > Size ~p", [ValueSize, Size]),
            over;
        true ->
            ok
    end.

lookup(ETSName, Key)->
    case ets:lookup(ETSName, Key) of
        [] ->
            {error, not_found};
        [Cache | _Tail] ->
            CurrentTime = current_time(),
            if
                Cache#cache.ttl + Cache#cache.start_time =< CurrentTime ->
                    ets:delete(ETSName, Key),
                    {error, expired};
                true ->
                    ets:insert(ETSName, Cache#cache{
                                        start_time = CurrentTime, 
                                        access_cnt = Cache#cache.access_cnt+1
                                        }),
                    {ok, Cache#cache.val}
            end
    end.

delete_expired(ETSName) ->
    CurrentTime = current_time(),
    ExpireCond = ets:fun2ms(fun(#cache{start_time=StartTime, ttl=TTL}) when StartTime + TTL =< CurrentTime -> true end),
    ?debug("MatchSpec:~p", [ExpireCond]),
    ets:select_delete(ETSName, ExpireCond).


sortkey_by_time(ETSName, Weight) ->
    Wgts = ets:foldl(
        fun(Cache, Acc) ->
            [{Cache#cache.key, Cache#cache.access_cnt*Weight + Cache#cache.start_time + Cache#cache.ttl} | Acc]
        end, [], ETSName),
    CmpFun = fun({_, Time1}, {_, Time2}) ->
        if
            Time1 < Time2 -> true;
            true -> false
        end
    end,
    lists:sort(CmpFun, Wgts).


delete_light_items(ETSName, ThresholdSize, Weight) ->
    case check_memsize(ETSName, ThresholdSize) of
        over ->
            SortedKeys = sortkey_by_time(ETSName, Weight),
            % ?debug("sorted ~p", [SortedKeys]),
            delete_items(SortedKeys, ETSName, ThresholdSize);
        _ ->
            ok
    end.


delete_items([], _ETSName, _ThresholdSize) ->
    ok;
delete_items([{Key, _}|Tail], ETSName, ThresholdSize) ->
    ?warning("ets over ThresholdSize:~p delete_item Key:~p", [ThresholdSize, Key]),
    ets:delete(ETSName, Key),
    case check_memsize(ETSName, ThresholdSize) of
        over ->
            delete_items(Tail, ETSName, ThresholdSize);
        _ ->
            ok
    end.


