-module(hoax_code).

-export([
        get_function_list/1,
        get_function_list/2,
        expectation_list_to_function_list/2,
        purge_and_delete/1
    ]).

get_function_list(ModuleName) ->
    module_exists(ModuleName) orelse
        error({no_such_module_to_mock, ModuleName}),
    [E || E = {F,_} <- ModuleName:module_info(exports), F =/= module_info].

get_function_list(Behaviour, ModuleName) ->
    module_exists(ModuleName) andalso
        error({module_exists, ModuleName}),
    module_exists(Behaviour) orelse
        error({no_such_behaviour_to_mock, Behaviour}),
    erlang:function_exported(Behaviour, behaviour_info, 1) orelse
        error({not_a_behaviour, Behaviour}),
    Behaviour:behaviour_info(callbacks).

expectation_list_to_function_list(ModuleName, Expectations) ->
    module_exists(ModuleName) andalso
        error({module_exists, ModuleName}),
    [ element(1,X) || X <- Expectations ].

purge_and_delete(ModuleName) ->
    code:purge(ModuleName),
    code:delete(ModuleName).

module_exists(ModuleName) ->
    case code:ensure_loaded(ModuleName) of
        {error, nofile} -> false;
        {module, ModuleName} -> true
    end.
