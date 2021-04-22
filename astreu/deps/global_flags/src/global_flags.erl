%% @doc Write once global flags for Erlang and Elixir.
%%
%% This module provides a very simple API for managing global
%% flags and conditional execution based on those flags. This
%% is designed for setups where you need quick flag checks
%% before execution, but don't want to waste a process, start
%% an ETS table, and cannot rely on the process dictionary.
%%
%% It works by using a super simple atom table check, which
%% makes the check almost instant (and without having to rely
%% on any prior state).
%%
%% Flags cannot be unset after being set, due to the inability
%% to purge from the atom table. If you want such ability, you
%% likely want to deal with ETS or some other storage.
-module(global_flags).
-compile(inline).

%% Public API
-export([is_set/1, once/2, set/1, with/2, without/2]).

%% ===================================================================
%% Public API
%% ===================================================================

%% @doc Checks if a global flag is set.
-spec is_set(binary() | list()) -> boolean().
is_set(Flag) when is_binary(Flag) ->
    try binary_to_existing_atom(Flag, utf8) of
        _ -> true
    catch
        _:_ -> false
    end;
is_set(Flag) when is_list(Flag) ->
    try list_to_existing_atom(Flag) of
        _ -> true
    catch
        _:_ -> false
    end.

%% @doc Runs a function a single time, based on the provided flag.
-spec once(binary() | list(), fun(() -> any())) -> any | {error, flag_state}.
once(Flag, Fun) when
    is_binary(Flag); is_list(Flag),
    is_function(Fun, 0)
->
    case is_set(Flag) of
        true    -> {error, flag_state};
        false   ->
            Result = Fun(),
            set(Flag),
            Result
    end.

%% @doc Sets a global flag, typically only used internally.
-spec set(binary() | list()) -> ok.
set(Flag) when is_binary(Flag) ->
    _ = binary_to_atom(Flag, utf8),
    ok;
set(Flag) when is_list(Flag) ->
    _ = list_to_atom(Flag),
    ok.

%% @doc Runs a function only if the provided flag is set.
-spec with(binary() | list(), fun(() -> any())) -> any | {error, flag_state}.
with(Flag, Fun) when
    is_binary(Flag); is_list(Flag),
    is_function(Fun, 0)
->
    case is_set(Flag) of
        false   -> {error, flag_state};
        true    -> Fun()
    end.

%% @doc Runs a function only if the provided flag is not set.
-spec without(binary() | list(), fun(() -> any())) -> any | {error, flag_state}.
without(Flag, Fun) when
    is_binary(Flag); is_list(Flag),
    is_function(Fun, 0)
->
    case is_set(Flag) of
        true    -> {error, flag_state};
        false   -> Fun()
    end.

%% ===================================================================
%% Private test cases
%% ===================================================================

-ifdef(TEST).
    -include_lib("eunit/include/eunit.hrl").

    set_test() ->
        Flag = "test_set",
        ?assertNot(global_flags:is_set(Flag)),
        ?assert(global_flags:set(Flag) =:= ok),
        ?assert(global_flags:is_set(Flag)),
        ok.

    once_test() ->
        Flag = "test_once",
        Once1 = global_flags:once(Flag, fun() ->
            woohoo
        end),
        Once2 = global_flags:once(Flag, fun() ->
            woohoo
        end),
        ?assert(Once1 =:= woohoo),
        ?assert(Once2 =:= {error, flag_state}).

    with_test() ->
        Flag = "test_with",
        Handle = fun() -> woohoo end,
        With1 = global_flags:with(Flag, Handle),
        global_flags:set(Flag),
        With2 = global_flags:with(Flag, Handle),
        ?assert(With1 =:= {error, flag_state}),
        ?assert(With2 =:= woohoo).

    without_test() ->
        Flag = "test_without",
        Handle = fun() -> woohoo end,
        Without1 = global_flags:without(Flag, Handle),
        global_flags:set(Flag),
        Without2 = global_flags:without(Flag, Handle),
        ?assert(Without1 =:= woohoo),
        ?assert(Without2 =:= {error, flag_state}).
-endif.
