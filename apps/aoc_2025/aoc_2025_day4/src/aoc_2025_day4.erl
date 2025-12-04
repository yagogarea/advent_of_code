-module(aoc_2025_day4).

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
    Matrix = aoc_utils_matrices:list_to_matrix(string:lexemes(Input, [$\n, 32])),
    sol_aux(Matrix, maps:to_list(Matrix), 0).

-spec sol2(Input) -> Result when
    Input :: term(),
    Result :: term().
sol2(Input) ->
    Matrix = aoc_utils_matrices:list_to_matrix(string:lexemes(Input, [$\n, 32])),
    remove_elements(Matrix, 0).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
sol_aux(_Matrix, [], Acc) ->
    Acc;
sol_aux(Matrix, [{_Pos, 46} | T], Acc) ->
     sol_aux(Matrix, T, Acc);
sol_aux(Matrix, [{{PosX, PosY}, 64} | T], Acc) ->
    Adjacent = aoc_utils_matrices:adjacent_elements(PosX, PosY, Matrix),
    case length(lists:filter(fun(Simbol) -> Simbol == 64 end, Adjacent)) of
        Number when Number < 4 ->
            sol_aux(Matrix, T, Acc + 1);
        _ ->
            sol_aux(Matrix, T, Acc)
    end.

remove_elements(Matrix, Acc) ->
    MatrixList = maps:to_list(Matrix),
    {NewAcc, NewToRemove} = sol2_aux(Matrix, MatrixList, [], Acc),
    case NewToRemove of
        [] ->
            NewAcc;
        _ ->
            NewMatrix = lists:foldl(fun(Pos, MatrixAcc) -> maps:put(Pos, $x, MatrixAcc) end, Matrix, NewToRemove),
            remove_elements(NewMatrix, NewAcc)
    end.

sol2_aux(_Matrix, [], NextRemoved, Acc) ->
    {Acc, NextRemoved};
sol2_aux(Matrix, [{{PosX, PosY}, 64} | T], NextRemoved, Acc) ->
    Adjacent = aoc_utils_matrices:adjacent_elements(PosX, PosY, Matrix),
    case length(lists:filter(fun(Simbol) -> Simbol == 64 end, Adjacent)) of
        Number when Number < 4 ->
            NewToRemove = [{PosX, PosY} | NextRemoved],
            sol2_aux(Matrix, T, NewToRemove, Acc + 1);
        _ ->
            sol2_aux(Matrix, T, NextRemoved, Acc)
    end;
sol2_aux(Matrix, [{_Pos, _} | T], NextRemoved, Acc) ->
     sol2_aux(Matrix, T, NextRemoved, Acc).
