-module(ecache_test).
-include_lib("eunit/include/eunit.hrl").


%% ===================================================================
%% TESTS DESCRIPTIONS
%% ===================================================================




%% ===================================================================
%% SETUP FUNCTIONS
%% ===================================================================
setup() ->

    application:load(lager),
    application:set_env(lager, handlers, [
        {lager_console_backend,
            [debug, {lager_default_formatter, [color, time, " ", pid, " [", severity, "] [", {module, [module,
                {function, [":", function], ""},
                {line, [":", line], ""}], ""}, "] ", message, "\n"]}]}]),
    application:set_env(lager, error_logger_redirect, true),
    application:set_env(lager, colored, true),

    application:set_env(ecache, ets_threshold, 0.85),
    application:set_env(ecache, ets_maxsize, 5*1024),
    
    ecache:start().

cleanup(_) ->
    ecache:stop(),
    application:stop(lager).

all_test_() ->
   
    {setup, fun setup/0, fun cleanup/1,
        [
            ?_assertMatch({error, not_found}, ecache:get(<<"key1">>)),
            ?_assertMatch(ok, ecache:set(<<"key1">>, <<"value1">>, 2)),
            ?_assertMatch({ok, <<"value1">>}, ecache:get(<<"key1">>)),
            ?_assertMatch(ok, ecache:set(<<"key1">>, <<"value2">>, 2)),
            ?_assertMatch({ok, <<"value2">>}, ecache:get(<<"key1">>)),
            ?_assertMatch(ok, ecache:delete(<<"key1">>)),
            ?_assertMatch({error, not_found}, ecache:get(<<"key1">>))
        ]
    }.



%% ===================================================================
%% TEST CASES
%% ===================================================================




%% ===================================================================
%% HELPER FUNCTIONS
%% ===================================================================
