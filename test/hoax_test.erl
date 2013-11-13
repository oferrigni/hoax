-module(hoax_test).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("hoax/include/hoax.hrl").

stop_should_unload_all_hoaxed_modules_test() ->
    ExpectedResult = hoax_test_module:function_one(1, 2),
    hoax:test(fun() ->
        hoax:expect_no_interactions(no_such_module),
        hoax:mock(
            hoax_test_module:function_one(1, 2) > mocked_return_value
        ),
        ?assertEqual(mocked_return_value, hoax_test_module:function_one(1, 2))
    end),

    Result = hoax_test_module:function_one(1, 2),
    ?assertEqual(ExpectedResult, Result),

    ?assertMatch({error, nofile}, code:ensure_loaded(no_such_module)).

should_be_able_to_mock_sticky_modules_test() ->
    ExpectedResult = hoax_test_module:function_one(1, 2),
    code:stick_mod(hoax_test_module),
    try
        hoax:test(fun() ->
            hoax:mock(
                hoax_test_module:function_one(1, 2) > mocked_return_value
            ),
            ?assertEqual(mocked_return_value, hoax_test_module:function_one(1, 2)),
            ?assert(code:is_sticky(hoax_test_module))
        end),
        Result = hoax_test_module:function_one(1, 2),
        ?assertEqual(ExpectedResult, Result),
        ?assert(code:is_sticky(hoax_test_module))
    after
        code:unstick_mod(hoax_test_module)
    end.
