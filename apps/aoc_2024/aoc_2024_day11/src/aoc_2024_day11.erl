-module(aoc_2024_day11).

%%% BEHAVIOUR
-behaviour(aoc_puzzle).

%%% BEHAVIOUR CALLBACK EXPORTS
-export([
    sol/1,
    sol2/1,
    iterations/2
]).

%%%-----------------------------------------------------------------------------
%%% BEHAVIOUR CALLBACK EXPORTS
%%%-----------------------------------------------------------------------------
-spec sol(Input) -> Result when
    Input :: term(),
    Result :: term().
sol(Input) ->
    Stones = string:lexemes(Input, [32]),
    NewStones = iterations(25, Stones),
    length(NewStones).

-spec sol2(Input) -> Result when
    Input :: term(),
    Result :: term().
sol2(Input) ->
    Stones = string:lexemes(Input, [32]),
    NewStones = iterations2(75, Stones),
    lists:foldl(fun({_X, Count}, Acc) -> Acc + Count end, 0, NewStones).

%%%-----------------------------------------------------------------------------
%%% INTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
iterations(0, Stones) ->
    Stones;
iterations(N, Stones) ->
    io:format("Stones ~p ~p~n", [N, Stones]),
    iterations(N - 1, iteration(Stones)).

iteration(Stones) ->
    iteration(Stones, []).

iteration([], Acc) ->
    lists:reverse(Acc);
iteration(["0" | T], Acc) ->
    iteration(T, ["1" | Acc]);
iteration([H | T], Acc) ->
    case length(H) rem 2 == 0 of
        true ->
            {Stone1, Stone2} = lists:split(length(H) div 2, H),
            iteration(T, [remove_extra_leading_zeroes(Stone2), remove_extra_leading_zeroes(Stone1) | Acc]);
        false ->
            iteration(T, [integer_to_list(list_to_integer(H) * 2024) | Acc])
    end.

iterations2(0, Stones) ->
    lists:reverse(Stones);
iterations2(N, Stones) ->
    Deduplicated = remove_duplicates_counting(Stones),
    iterations2(N - 1, iteration2(Deduplicated)).

iteration2(Stones) ->
    iteration2(Stones, []).

iteration2([], Acc) ->
    lists:reverse(Acc);
iteration2([{"0", Count} | T], Acc) ->
    iteration2(T, [{"1", Count} | Acc]);
iteration2([{X, Count} | T], Acc) ->
    case length(X) rem 2 == 0 of
        true ->
            {Stone1, Stone2} = lists:split(length(X) div 2, X),
            iteration2(T, [{remove_extra_leading_zeroes(Stone2), Count}, {remove_extra_leading_zeroes(Stone1), Count} | Acc]);
        false ->
            iteration2(T, [{integer_to_list(list_to_integer(X) * 2024), Count} | Acc])
    end.

remove_duplicates_counting(List) ->
    remove_duplicates_counting(List, []).

remove_duplicates_counting([], Acc) ->
    Acc;
remove_duplicates_counting([{H, Count} | T], Acc) ->
    Element = {H, count_tuple_key(T, H) + Count},
    remove_duplicates_counting(lists:filter(fun({X, _Value}) -> X =/= H end, T), [Element | Acc]);
remove_duplicates_counting([H | T], Acc) ->
    Element = {H, aoc_utils_lists:count(T, H) + 1},
    remove_duplicates_counting(lists:filter(fun(X) -> X =/= H end, T), [Element | Acc]).

count_tuple_key(List, Element) ->
    count_tuple_key(List, Element, 0).

count_tuple_key([], _Key, Acc) ->
    Acc;
count_tuple_key([{Key, Value} | T], Key, Acc) ->
    count_tuple_key(T, Key, Acc + Value);
count_tuple_key([_H | T], Key, Acc) ->
    count_tuple_key(T, Key, Acc).

remove_extra_leading_zeroes(X) ->
    integer_to_list(list_to_integer(X)).
