-module(hoax).

-export([start/0, stop/0, verify_all/2, test/1, nullary_fixture/4, unary_fixture/4, expect_no_interactions/1, mock/1]).
-ignore_xref([mock/2, stub/3]).
-include("hoax_int.hrl").

%% ===================================================================
%% hoax API
%% ===================================================================

start() ->
    hoax_tab:create().

stop() ->
    lists:foreach(fun hoax_code:purge_and_delete/1, hoax_tab:delete()).

mock(Expectations) ->
    hoax_tab:insert(Expectations),

    GroupBy = fun(Expectation, Dict) ->
        {Mod, _, _} = Expectation#expectation.key,
        dict:append(Mod, Expectation, Dict)
    end,
    ByModule = lists:foldl(GroupBy, dict:new(), Expectations),

    MockModule = fun({ModuleName, Records}) ->
        Exports = hoax_code:get_export_list(ModuleName, Records),
        Forms = hoax_module:generate(ModuleName, Exports),
        hoax_code:compile(ModuleName, Forms)
    end,
    lists:foreach(MockModule, dict:to_list(ByModule)).

test(TestFun) when is_function(TestFun) ->
    hoax:start(),
    try
        TestFun()
    after
        hoax:stop()
    end.

expect_no_interactions(Module) when is_atom(Module) ->
    hoax_tab:insert(#expectation{
        key = {Module, undefined, undefined},
        expected_count=0
    }).

%% @private
nullary_fixture(Module, Caller, Setup, Teardown) when is_function(Setup), is_function(Teardown) ->
    {foreach,
        fun() -> hoax:start(), Setup() end,
        fun(X) -> Teardown(X), hoax:stop() end,
        [ {Module, F} ||
            {F, 0} <- Module:module_info(exports),
            F =/= Caller,
            F =/= module_info,
            F =/= test
        ]
    };
nullary_fixture(_, _, _, _) ->
    erlang:error({badarg, "Setup and Teardown must be functions"}).

%% @private
unary_fixture(Module, Caller, Setup, Teardown) when is_function(Setup), is_function(Teardown) ->
    {foreach,
        fun() -> hoax:start(), Setup() end,
        fun(X) -> Teardown(X), hoax:stop() end,
        [ {with, [{Module, F}]} ||
            {F, 1} <- Module:module_info(exports),
            F =/= Caller,
            F =/= module_info,
            F =/= test
        ]
    };
unary_fixture(_, _, _, _) ->
    erlang:error({badarg, "Setup and Teardown must be functions"}).

%% @private
verify_all(Module, Line) ->
    case hoax_tab:unmet_expectations() of
        []  -> ok;
        Unmet -> erlang:error({unmet_expectations,
            [{module, Module},
                {line, Line},
                {expected, Unmet}
            ]})
    end.
