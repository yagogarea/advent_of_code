-module(aoc_2024_day4).

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
    X = lists:filter(fun({{_X, _Y}, Value}) -> Value == 88 end, MatrixList),
    XM = find_pattern(Matrix, X, 77),
    XMA = find_pattern_keep(Matrix, lists:flatten(XM), 65),
    XMAS = find_pattern_keep(Matrix, XMA, 83),
    length(XMAS).

-spec sol2(Input) -> Result when
    Input :: term(),
    Result :: term().
sol2(Input) ->
    Matrix = aoc_utils_matrices:list_to_matrix(string:lexemes(Input, [$\n])),
    MatrixList = maps:to_list(Matrix),
    A = lists:filter(fun({{_X, _Y}, Value}) -> Value == 65 end, MatrixList),
    List= lists:filter(
        fun({{X, Y}, _Value}) ->
            case aoc_utils_matrices:get(X + 1, Y + 1, Matrix, undefined) of
                83 ->
                    case aoc_utils_matrices:get(X - 1, Y - 1, Matrix, undefined) of
                       77 ->
                            First = true;
                        _ ->
                            First = false
                    end;
                77 ->
                    case aoc_utils_matrices:get(X - 1, Y - 1, Matrix, undefined) of
                       83 ->
                            First = true;
                        _ ->
                            First = false
                    end;
                _ ->
                    First = false
            end,
            case aoc_utils_matrices:get(X + 1, Y - 1, Matrix, undefined) of
                83 ->
                    case aoc_utils_matrices:get(X - 1, Y + 1, Matrix, undefined) of
                       77 ->
                            Second = true;
                        _ ->
                            Second = false
                    end;
                77 ->
                    case aoc_utils_matrices:get(X - 1, Y + 1, Matrix, undefined) of
                       83 ->
                            Second = true;
                        _ ->
                            Second = false
                    end;
                _ ->
                    Second = false
            end,
            First andalso Second
        end, A),
    length(List).


find_pattern(Matrix, MatrixList, V2) ->
    lists:foldl(
            fun({{X, Y}, _Value}, List) -> 
                AdjacentsX = aoc_utils_matrices:adjacent_elements_with_pos(X, Y, Matrix),
                % AdjacentsVal = string:lexemes(lists:map(fun({{_X, _Y}, V}) -> V end), Adjacents),
                M = lists:foldl(
                    fun({{X1, Y1}, Value2}, List2) -> 
                        case Value2 of
                            V2 ->
                                [{X1 - X, Y1 - Y, {{X1, Y1}, Value2}} | List2];
                            _ ->
                                List2
                        end
                    end, 
                    [],
                    AdjacentsX
                ),
                [M | List]
            end,
            [],
            MatrixList
        ).


find_pattern_keep(Matrix, XM, V) ->
    lists:foldl(
        fun({X, Y,{{X1,Y1},_}}, Acc2) ->
            PosX = X1 + X,
            PosY =  Y1 + Y,
            case aoc_utils_matrices:get(PosX, PosY, Matrix, undefined) of
                    V ->
                        [{X, Y, {{PosX, PosY}, V}} | Acc2];
                    _Else ->
                        Acc2
            end
        end, 
        [], 
        XM
    ).
