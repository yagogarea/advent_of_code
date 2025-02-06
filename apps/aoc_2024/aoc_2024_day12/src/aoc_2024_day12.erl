-module(aoc_2024_day12).

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
    Groups = matrix_groups(Matrix, MatrixList, []),
    lists:foldl(
        fun({Group, Perimeter}, Acc) ->
            Perimeter * length(Group) + Acc
        end, 
        0, 
        Groups
    ).

-spec sol2(Input) -> Result when
    Input :: term(),
    Result :: term().
sol2(Input) ->
    Matrix = aoc_utils_matrices:list_to_matrix(string:lexemes(Input, [$\n, 32])),
    MatrixList = maps:to_list(Matrix),
    Groups = matrix_groups2(Matrix, MatrixList, []),
    lists:foldl(
        fun({Group, Perimeter}, Acc) ->
            {Rows, Columns} = lists:foldl(
                fun({X, Y, Value}, {Rows, Columns}) ->  
                    case Value of
                        "-" ->
                            {maps:put(X, [Y | maps:get(X, Rows, [])], Rows), Columns};
                        "|" ->
                            {Rows, maps:put(Y, [X | maps:get(Y, Columns, [])], Columns)}
                    end
                end,
                {#{}, #{}},    
                Perimeter
            ),
            Sides = lists:foldl(
                fun({_, List}, Acc2) ->
                    Acc2 + aux(List)
                end,
                0,
                maps:to_list(Rows) ++ maps:to_list(Columns)
            ),
            length(Group) * Sides + Acc
        end, 
        0, 
        Groups
    ).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
matrix_groups(_Matrix, [], Acc) ->
    Acc;
matrix_groups(Matrix, [{{X, Y}, Value} | T], Acc) ->
    {Group, Perimeter} = equal_neighbors(Matrix, {{X, Y}, Value}, 0),
    NewT = lists:filter(fun(Elem) -> lists:member(Elem, Group) == false end, T),
    matrix_groups(Matrix, NewT, [{Group, Perimeter}] ++ Acc).

equal_neighbors(Matrix, {{X, Y}, Value}, Perimeter) ->
    equal_neighbors(Matrix, [{{X, Y}, Value}], Perimeter, []).
    
equal_neighbors(_Matrix, [], Perimeter, Acc) ->
    {Acc, Perimeter};
equal_neighbors(Matrix, [{{X, Y}, Value} | T], Perimeter, Acc) ->
    Dirs = 
    [
        {X + 1, Y}, {X, Y + 1},
        {X - 1, Y}, {X, Y - 1}
    ],
    
    Explored = lists:foldl(
            fun({Row, Column}, Acc2) -> 
                case aoc_utils_matrices:get(Row, Column, Matrix, undefined) == Value of
                    true ->
                        [{{Row, Column}, Value} | Acc2];
                    false ->
                        Acc2
                end
            end,
            [], 
            Dirs
        ), 
    News = lists:filter(fun(Elem) -> lists:member(Elem, Acc) == false end, Explored),
    equal_neighbors(Matrix, News ++ T, Perimeter + 4 - length(Explored), aoc_utils_lists:union([{{X, Y}, Value} | News], Acc)).


matrix_groups2(_Matrix, [], Acc) ->
    Acc;
matrix_groups2(Matrix, [{{X, Y}, Value} | T], Acc) ->
    {Group, Perimeter} = equal_neighbors2(Matrix, {{X, Y}, Value}, []),
    NewT = lists:filter(fun(Elem) -> lists:member(Elem, Group) == false end, T),
    matrix_groups2(Matrix, NewT, [{Group, Perimeter}] ++ Acc).

equal_neighbors2(Matrix, {{X, Y}, Value}, Perimeter) ->
    equal_neighbors2(Matrix, [{{X, Y}, Value}], Perimeter, []).
    
equal_neighbors2(_Matrix, [], Perimeter, Acc) ->
    {Acc, Perimeter};
equal_neighbors2(Matrix, [{{X, Y}, Value} | T], Perimeter, Acc) ->
    Dirs = 
    [
        {X + 1, Y}, {X, Y + 1},
        {X - 1, Y}, {X, Y - 1}
    ],
    {Explored, Borders} = lists:foldl(
            fun({Row, Column}, {Acc2, Acc3}) -> 
                case aoc_utils_matrices:get(Row, Column, Matrix, undefined) == Value of
                    true ->
                        {[{{Row, Column}, Value} | Acc2], Acc3};
                    false ->
                        case {Row, Column} of
                            {Row, Column} when Y < Column ->
                                {Acc2, [{Row, Column - 0.25, "|"} | Acc3]};
                            {Row, Column} when Y > Column ->
                                {Acc2, [{Row, Column + 0.25, "|"} | Acc3]};
                            {Row, Column} when X < Row ->
                                {Acc2, [{Row - 0.25, Column, "-"} | Acc3]};
                            {Row, Column} when X > Row ->
                                {Acc2, [{Row + 0.25, Column, "-"} | Acc3]}
                        end
                end
            end,
            {[], []}, 
            Dirs
        ), 
    
    News = lists:filter(fun(Elem) -> lists:member(Elem, Acc) == false end, Explored),
    NewPerimeter = 
    lists:filter(
        fun({Row, Column, Type}) -> 
            lists:member({Row, Column, Type}, Perimeter) == false 
        end,
        Borders
    ),
    equal_neighbors2(Matrix, News ++ T, aoc_utils_lists:union(Perimeter, NewPerimeter), aoc_utils_lists:union([{{X, Y}, Value} | News], Acc)).


aux(List) ->
    aux(lists:sort(List), 1).

aux([], Acc) ->
    Acc;
aux([_H], Acc) ->
    Acc;
aux([H1, H2 | T], Acc) when H2 == H1 + 1 ->
    aux([H2 | T], Acc);
aux([_H1, H2 | T], Acc) ->
    aux([H2 | T], Acc + 1).
    