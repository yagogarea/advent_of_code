-module(aoc_2024_day10).

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
    MatrixList = maps:to_list(Matrix),
    PossibleTrailheads = [{{X, Y}, Value} || {{X, Y}, Value} <- MatrixList, Value == 48],
    lists:foldl(
        fun(Element, Acc) -> 
            Path = check_trailhead(Matrix, MatrixList, Element, []),
            length(lists:usort(Path)) + Acc end,
            0,
            PossibleTrailheads
    ).

-spec sol2(Input) -> Result when
    Input :: term(),
    Result :: term().
sol2(Input) ->
    Matrix = aoc_utils_matrices:list_to_matrix(string:lexemes(Input, [$\n, 32])),
    MatrixList = maps:to_list(Matrix),
    PossibleTrailheads = [{{X, Y}, Value} || {{X, Y}, Value} <- MatrixList, Value == 48],
    lists:foldl(
        fun(Element, Acc) -> 
            NineList = check_trailhead(Matrix, MatrixList, Element, []),
            length(NineList) + Acc
        end,
        0,
        PossibleTrailheads
    ).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
check_trailhead(_Matrix, _MatrixList, {{X, Y}, 57}, Acc) ->
    aoc_utils_lists:union(Acc, [{{X, Y}, 57}]);
check_trailhead(Matrix, _MatrixList, {{X, Y}, Value}, Acc) ->
    Dirs = 
    [
        {X + 1, Y}, {X, Y + 1},
        {X - 1, Y}, {X, Y - 1}
    ],
    Nexts = [
        {{PosX, PosY}, Value + 1} || 
        {PosX, PosY} <- Dirs, 
        aoc_utils_matrices:get(PosX, PosY, Matrix, undefined) == Value + 1 
    ],
    case Nexts of
        [] ->
            [];
        [Element] ->
            check_trailhead(Matrix, _MatrixList, Element, Acc);
        _List ->
            lists:flatten(lists:map(fun({{PosX, PosY}, Val}) -> check_trailhead(Matrix, _MatrixList, {{PosX, PosY}, Val}, Acc) end, Nexts))
    end.
