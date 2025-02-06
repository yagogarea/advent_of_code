-module(aoc_2024_day1).

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
    Lexemes = string:lexemes(Input, [$\n, 32]),
    {List1, List2} = split_in_two_sets(Lexemes),
    sol_aux(List1, List2).


-spec sol2(Input) -> Result when
    Input :: term(),
    Result :: term().
sol2(Input) ->
    Lexemes = string:lexemes(Input, [$\n, 32]),
    {List1, List2} = split_in_two_sets(Lexemes),
    sol2_aux(List1, List2).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
split_in_two_sets(Lexemes) ->
    split_in_two_sets(Lexemes, [], []).

split_in_two_sets([], List1, List2) ->
    {lists:sort(List1), lists:sort(List2)};
split_in_two_sets([E1, E2 | Rest], List1, List2) ->
    split_in_two_sets(Rest, [list_to_integer(E1) | List1], [list_to_integer(E2) | List2]).

sol_aux(List1, List2) ->
    sol_aux(List1, List2, 0).

sol_aux([], [], Acc) ->
    Acc;
sol_aux([X1 | Rest1], [X2 | Rest2], Acc) ->
    case X1 - X2 of
        Negative when Negative < 0 ->
            NewAcc = Negative * -1 + Acc;
        Positive ->
            NewAcc = Positive + Acc
    end,
    sol_aux(Rest1, Rest2, NewAcc).

sol2_aux(List1, List2) ->
    sol2_aux(List1, List2, 0).

sol2_aux([], _List, Acc) ->
        Acc;
sol2_aux([X1 | Rest1], List, Acc) ->
    Condition = fun(X) -> X == X1 end,
    sol2_aux(Rest1, List, X1 * aoc_utils_lists:how_many(Condition, List) + Acc).
