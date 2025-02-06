-module(aoc_2024_day7).

%%% BEHAVIOUR
-behaviour(aoc_puzzle).

%%% BEHAVIOUR CALLBACK EXPORTS
-export([
    sol/1,
    sol2/1
]).

%%%-----------------------------------------------------------------------------
%%% BEHAVIOUR CALLBACK EXPORTS
%%%-----------------------------------------------------------------------------
-spec sol(Input) -> Result when
    Input :: term(),
    Result :: term().
sol(Input) ->
    Lines = string:lexemes(Input, [$\n]),
    Rules = lists:foldl(fun(Line, Acc) -> Rule = string:lexemes(Line, [$:]), [Rule | Acc] end, [], Lines),
    find(Rules).


-spec sol2(Input) -> Result when
    Input :: term(),
    Result :: term().
sol2(Input) ->
    Lines = string:lexemes(Input, [$\n]),
    Rules = lists:foldl(fun(Line, Acc) -> Rule = string:lexemes(Line, [$:]), [Rule | Acc] end, [], Lines),
    find2(Rules).

find(Rules) ->
    find(Rules, 0).

find([], Acc) ->
    Acc;
find([[TestValue, Equation] | T], Acc) ->
    TestValueNumber = list_to_integer(TestValue),
    EquationNumbers = string:lexemes(Equation, [32]),
    case calc(TestValueNumber, EquationNumbers) of
        false ->
            find(T, Acc);
        true ->
            find(T, Acc + TestValueNumber)
    end.

calc(TestValue, [H | T]) ->
    calc(TestValue, T, list_to_integer(H)) .

calc(TestValue, [], TestValue) ->
    true;
calc(_TestValue, [], _Acc) ->
    false;
calc(TestValue, [H | T], Acc) ->
    calc(TestValue, T, Acc + list_to_integer(H)) orelse
    calc(TestValue, T, Acc * list_to_integer(H)).

find2(Rules) ->
    find2(Rules, 0).

find2([], Acc) ->
    Acc;
find2([[TestValue, Equation] | T], Acc) ->
    TestValueNumber = list_to_integer(TestValue),
    EquationNumbers = string:lexemes(Equation, [32]),
    case calc2(TestValueNumber, EquationNumbers) of
        false ->
            find2(T, Acc);
        true ->
            find2(T, Acc + TestValueNumber)
    end.

calc2(TestValue, [H | T]) ->
    calc2(TestValue, T, list_to_integer(H)) .

calc2(TestValue, [], TestValue) ->
    true;
calc2(_TestValue, [], _Acc) ->
    false;
calc2(TestValue, [H | T], Acc) ->
    calc2(TestValue, T, Acc + list_to_integer(H)) orelse
    calc2(TestValue, T, Acc * list_to_integer(H)) orelse
    calc2(TestValue, T, list_to_integer(integer_to_list(Acc) ++ H)).
