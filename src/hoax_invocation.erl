-module(hoax_invocation).

-export([handle/3]).
-include("hoax_int.hrl").

handle(M, F, Args) ->
    case hoax_tab:lookup({M, F, length(Args)}) of
        [] ->
            erlang:error({unexpected_invocation, fmt({M, F, Args})});
        Records ->
            case find_matching_args(Args, Records) of
                false ->
                    erlang:error({unexpected_arguments, fmt({M, F, Args})});
                #expectation{call_count=X,expected_count=X,args=ExpectedArgs} ->
                    erlang:error({too_many_invocations, X+1, fmt({M, F, ExpectedArgs})});
                #expectation{action = Action} = Record ->
                    hoax_tab:increment_counter(Record),
                    perform(Action)
            end
    end.

find_matching_args(Args, Records) ->
    keyfind(Args, Records).

keyfind(ActualArgs, [ Expectation = #expectation{args = ExpectedArgs} | Rest ]) ->
    case replace_wildcards(ActualArgs, ExpectedArgs) of
        ActualArgs -> Expectation;
        _          -> keyfind(ActualArgs, Rest)
    end;
keyfind(_, []) ->
    false.

perform(default)         -> '$_hoax_default_return_$';
perform(Fun) -> Fun().

replace_wildcards(ActualArgs, ExpectedArgs) ->
    lists:zipwith(fun replace_wildcard/2, ActualArgs, ExpectedArgs).

replace_wildcard(Actual, '_') -> Actual;
replace_wildcard(_, Expected) -> Expected.

fmt(#expectation{key = {M,F,_}, args=Args}) ->
    fmt({M, F, Args});
fmt({M, F, Args}) ->
    lists:flatten(io_lib:format("~s:~s(~s)", [M, F, format_args(Args)])).

format_args(Args) ->
    Formatted = lists:flatten(io_lib:format("~p", [Args])),
    string:sub_string(Formatted, 2, length(Formatted) - 1).
