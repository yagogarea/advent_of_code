-module(aoc_2025_day5).

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
    [Ranges, Numbers] = string:split(Input, "\n\n"),
    CleanRanges = string:lexemes(Ranges, [$\n,$-]),
    CleanNumbers = string:lexemes(Numbers, [$\n]),
    sol_aux(CleanNumbers, CleanRanges).

-spec sol2(Input) -> Result when
    Input :: term(),
    Result :: term().
sol2(Input) ->
    [Ranges, _Numbers] = string:split(Input, "\n\n"),
    CleanRanges = lists:map(fun list_to_integer/1, string:lexemes(Ranges, [$\n,$-])),
    DiffRanges = sol2_aux(CleanRanges, #{}),
    sum_ranges(DiffRanges, 0).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
sol_aux(Numbers, Ranges) ->
    sol_aux(Numbers, Ranges, 0).

sol_aux([], _Ranges, Acc) ->
    Acc;
sol_aux([N | Numbers], Ranges, Acc) ->
    case is_inside_ranges(list_to_integer(N), Ranges) of
        true ->
            sol_aux(Numbers, Ranges, Acc + 1);
        false ->
            sol_aux(Numbers, Ranges, Acc)
    end.

is_inside_ranges(_N, []) ->
    false;
is_inside_ranges(N, [N1, N2 | T]) ->
    case N >= list_to_integer(N1) andalso list_to_integer(N2) >= N of
        true ->
            true;
        false ->
            is_inside_ranges(N, T)
    end.
    
sol2_aux([], Acc) ->
    update_ranges(lists:usort(maps:to_list(Acc)), []);
sol2_aux([N1, N2 | T], Acc) ->
    case maps:get(N1, Acc, undefined) of
        undefined ->
            sol2_aux(T, maps:put(N1, N2, Acc));
        Element when N2 > Element ->
             sol2_aux(T, maps:put(N1, N2, Acc));
        _ ->
            sol2_aux(T, Acc)
    end.

update_ranges([{N1, N2}], Acc) ->
    lists:reverse([{N1, N2} | Acc]);
update_ranges([{N1, N2}, {_D1, D2} | T], Acc) when N2 >= D2 ->
    update_ranges([{N1, N2} | T], Acc);
update_ranges([{N1, N2}, {D1, D2} | T], Acc) when N2 >= D1 ->
     update_ranges([{N1, D1 - 1}, {D1, D2} | T], Acc);
update_ranges([{N1, N2}, {D1, D2} | T], Acc) when N2 < D1 ->
    update_ranges([{D1, D2} | T], [{N1, N2} | Acc]).

sum_ranges([], Acc) ->
    Acc;
sum_ranges([{N1, N2} | Ranges], Acc) ->
    sum_ranges(Ranges, N2 - N1 + 1 + Acc).
