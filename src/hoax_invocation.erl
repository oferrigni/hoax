-module(hoax_invocation).

-export([handle/3]).
-include("hoax_int.hrl").

handle(M, F, Args) ->
    case hoax_tab:lookup_expectations({M, F, length(Args)}) of
        [] ->
            erlang:error({unexpected_invocation, {M, F, Args}});
        Records ->
            case find_matching_args(Args, Records) of
                false ->
                    erlang:error({unexpected_arguments, {M, F, Args}});
                #expectation{call_count=X,expected_count=X} ->
                    erlang:error({too_many_invocations, {M, F, Args}});
                #expectation{action = Action} = Record ->
                    hoax_tab:increment_counter(Record),
                    perform(Action)
            end
    end.

find_matching_args(Args, Records) ->
    lists:keyfind(Args, #expectation.args, Records).

perform(default)         -> '$_hoax_default_return_$';
perform({return, Value}) -> Value;
perform({Error, Reason}) -> erlang:Error(Reason).
