-module(aoc_2025_day3).

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
    Banks = string:lexemes(Input, [$\n]),
    sol_aux(Banks, 0).

-spec sol2(Input) -> Result when
    Input :: term(),
    Result :: term().
sol2(Input) ->
    Banks = string:lexemes(Input, [$\n]),
    sol2_aux(Banks, 0).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
sol_aux([], Acc) ->
    Acc;
sol_aux([Bank | Banks], Acc) ->
    [N1, N2] = find_biggers(Bank, [0, 0]),
    B1 = N1 - 48,
    B2 = N2 - 48,
    sol_aux(Banks, B1*10 + B2 + Acc).

find_biggers([], Acc) ->
    Acc;
find_biggers([Element], [N1, N2]) ->
    case Element > N2 of
        true ->
            find_biggers([], [N1, Element]);
        false ->
            find_biggers([], [N1, N2])
    end;
find_biggers([Element | T], [N1, N2]) ->
    case {Element > N1, Element > N2} of
        {true, _} ->
            find_biggers(T, [Element, 0]);
        {false, true} ->
            find_biggers(T, [N1, Element]);
        {false, false} ->
            find_biggers(T, [N1, N2])
    end.

sol2_aux([], Acc) ->
    Acc;
sol2_aux([Bank | Banks], Acc) ->
  Bigger = genius_aux_fun(Bank, 12, []),
  sol2_aux(Banks, list_to_integer(Bigger) + Acc).

genius_aux_fun(List, 1, Acc) ->
    [Great | _T] = lists:reverse(lists:usort(List)),
    lists:reverse(integer_to_list(Great - 48) ++ Acc);
genius_aux_fun(List, N, Acc) ->
    {Candidates, Rest} = lists:split(length(List) - N + 1, List),
    {Great, RestCandidates} = take_great_and_discard(Candidates),
    genius_aux_fun(RestCandidates ++ Rest, N - 1, integer_to_list(Great - 48) ++ Acc).

take_great_and_discard([H | T]) ->
    case lists:all(fun (X) -> H >= X end, T) of
        true ->
            {H, T};
        false ->
            take_great_and_discard(T)
    end.
