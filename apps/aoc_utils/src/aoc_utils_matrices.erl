-module(aoc_utils_matrices).

%%% EXTERNAL EXPORTS
-export([
    dimension/1,
    matrix/1,
    matrix/2,
    get/3,
    get/4,
    list_to_matrix/1,
    string_to_matrix/1,
    to_list/1,
    to_list_of_lists/1,
    adjacent_elements/3,
    adjacent_elements_pos/3,
    adjacent_elements_with_pos/3,
    corners_with_pos/3,
    matrix_groups/1,
    is_valid_coods/2,
    print_matrix/1,
    print_matrix/3,
    next_pos/4
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
dimension(Matrix) when is_map(Matrix) ->
    round(math:sqrt(maps:size(Matrix))).

matrix(N) ->
    build_matrix(N, N, 0).

matrix(N, M) ->
    build_matrix(N, M, 0).

get(X, Y, Matrix) ->
    maps:get({X, Y}, Matrix).

get(X, Y, Matrix, Default) ->
    maps:get({X, Y}, Matrix, Default).

list_to_matrix([H | T]) ->
    Columns = length(H),
    list_to_matrix(Columns, H, T).

string_to_matrix([H | T]) ->
    Columns = length(H),
    string_to_matrix(Columns, H, T).

to_list(Matrix) ->
    maps:to_list(Matrix).

to_list_of_lists(Matrix) ->
    to_list_of_lists(Matrix, 1, 1, []).

adjacent_elements(X, Y, Matrix) ->
    AdjacentPos = 
    [
        {X + 1, Y + 1}, {X + 1, Y}, {X + 1, Y - 1},
        {X, Y + 1}, {X, Y - 1},
        {X - 1, Y + 1}, {X - 1, Y}, {X - 1, Y - 1}
    ],
    adjacent_elements(AdjacentPos, Matrix).

adjacent_elements_pos(X, Y, Matrix) ->
    AdjacentPos = 
    [
        {X + 1, Y + 1}, {X + 1, Y}, {X + 1, Y - 1},
        {X, Y + 1}, {X, Y - 1},
        {X - 1, Y + 1}, {X - 1, Y}, {X - 1, Y - 1}
    ],
    adjacent_elements_pos(AdjacentPos, Matrix).

adjacent_elements_with_pos(X, Y, Matrix) ->
    AdjacentPos = 
    [
        {X + 1, Y + 1}, {X + 1, Y}, {X + 1, Y - 1},
        {X, Y + 1}, {X, Y - 1},
        {X - 1, Y + 1}, {X - 1, Y}, {X - 1, Y - 1}
    ],
    adjacent_elements_with_pos(AdjacentPos, Matrix).

corners_with_pos(X, Y, Matrix) ->
    AdjacentPos = 
    [
        {X + 1, Y + 1}, {X + 1, Y - 1},
        {X - 1, Y + 1}, {X - 1, Y - 1}
    ],
    adjacent_elements_with_pos(AdjacentPos, Matrix).

matrix_groups(Matrix) ->
    matrix_groups(Matrix, maps:to_list(Matrix), []).

is_valid_coods(MatrixDimension, {X, Y}) ->
    is_valid_cood(MatrixDimension, X) andalso is_valid_cood(MatrixDimension, Y).

print_matrix(Matrix) when is_map(Matrix) ->
    io:format(os:cmd(clear)),
    MatrixList = to_list_of_lists(Matrix),
    Format = print_matrix_aux(MatrixList, []),
    io:format(Format);
print_matrix(MatrixList) when is_list(MatrixList) ->
    io:format(os:cmd(clear)),
    Format = print_matrix_aux(MatrixList, []),
    io:format(Format).

print_matrix(Matrix, ValueZero, ValueElse) when is_map(Matrix) ->
    io:format(os:cmd(clear)),
    MatrixList = to_list_of_lists(Matrix),
    Format = print_matrix_aux(MatrixList, ValueZero, ValueElse, []),
    io:format(Format);
print_matrix(MatrixList, ValueZero, ValueElse) when is_list(MatrixList) ->
    io:format(os:cmd(clear)),
    Format = print_matrix_aux(MatrixList, ValueZero, ValueElse, []),
    io:format(Format).

next_pos({Px, Py, _Vx, _Vy}, _Rows, _Colums, 0) ->
    {Px, Py};
next_pos({Px, Py, Vx, Vy}, Rows, Columns, Steps) ->
    NewPx = new_coordinate(Px, Vx, Rows),
    NewPy = new_coordinate(Py, Vy, Columns),
    next_pos({NewPx, NewPy, Vx, Vy}, Rows, Columns, Steps - 1).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
adjacent_elements(AdjacentPos, Matrix) ->
    lists:foldl(
        fun({Row, Colum}, List) -> 
            case get(Row, Colum, Matrix, undefined) of
                undefined ->
                    List;
                Element -> 
                    [Element | List]
            end
        end,
        [],
    AdjacentPos
    ).

adjacent_elements_with_pos(AdjacentPos, Matrix) ->
    lists:foldl(
        fun({Row, Colum}, List) -> 
            case get(Row, Colum, Matrix, undefined) of
                undefined ->
                    List;
                Element -> 
                    [{{Row, Colum}, Element} | List]
            end
        end,
        [],
    AdjacentPos
    ).

adjacent_elements_pos(AdjacentPos, Matrix) ->
    lists:foldl(
        fun({Row, Colum}, List) -> 
            case get(Row, Colum, Matrix, undefined) of
                undefined ->
                    List;
                _Element -> 
                    [{Row, Colum} | List]
            end
        end,
        [],
    AdjacentPos
    ).

build_matrix(Rows, Columns, E) ->
    build_matrix(Rows, Columns, Columns, E, #{}).

build_matrix(0, _ColumnsCnt, _Columns, _E, Acc) ->
    Acc;
build_matrix(RowsCnt, 0, Columns, E, Acc) ->
    build_matrix(RowsCnt - 1, Columns, Columns, E, Acc);
build_matrix(RowsCnt, ColumnsCnt, Columns, E, Acc) ->
    build_matrix(RowsCnt, ColumnsCnt - 1, Columns, E, maps:put({RowsCnt, ColumnsCnt}, E, Acc)).

list_to_matrix(Columns, H, T) ->
    list_to_matrix(1, 1, Columns, H, T, #{}).

list_to_matrix(_, _ColumnsCnt, _Columns, [], [], Acc) ->
    Acc;
list_to_matrix(RowsCnt, _, Columns, [], [H2 | T2], Acc) ->
    list_to_matrix(RowsCnt + 1, 1, Columns, H2, T2, Acc);
list_to_matrix(RowsCnt, ColumnsCnt, Columns, [H1 | T1], T, Acc) ->
    list_to_matrix(RowsCnt, ColumnsCnt + 1, Columns, T1, T, maps:put({RowsCnt, ColumnsCnt}, H1, Acc)).


string_to_matrix(Columns, H, T) ->
    string_to_matrix(1, 1, Columns, H, T, #{}).

string_to_matrix(_, _ColumnsCnt, _Columns, [], [], Acc) ->
    Acc;
string_to_matrix(RowsCnt, _, Columns, [], [H2 | T2], Acc) ->
    string_to_matrix(RowsCnt + 1, 1, Columns, H2, T2, Acc);
string_to_matrix(RowsCnt, ColumnsCnt, Columns, [H1 | T1], T, Acc) ->
    string_to_matrix(RowsCnt, ColumnsCnt + 1, Columns, T1, T, maps:put({RowsCnt, ColumnsCnt}, [H1], Acc)).
    
matrix_groups(_Matrix, [], Acc) ->
    Acc;
matrix_groups(Matrix, [{{X, Y}, Value} | T], Acc) ->
    Group = equal_neighbors(Matrix, {{X, Y}, Value}),
    NewT = lists:filter(fun(Elem) -> lists:member(Elem, Group) == false end, T),
    matrix_groups(Matrix, NewT, [Group] ++ Acc).
    
equal_neighbors(Matrix, {{X, Y}, Value}) ->
    equal_neighbors(Matrix, [{{X, Y}, Value}], []).
    
equal_neighbors(_Matrix, [], Acc) ->
    Acc;
equal_neighbors(Matrix, [{{X, Y}, Value} | T], Acc) ->
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
    equal_neighbors(Matrix, News ++ T, aoc_utils_lists:union([{{X, Y}, Value} | News], Acc)).
    
is_valid_cood(MatrixDimension, X) ->
    case X of
        Number when Number < 1 ->
            false;
        Number when Number > MatrixDimension ->
            false;
        _Cood ->
            true
    end.

new_coordinate(P, V, Limit) ->
    case P + V of
        P2 when P2 < 1 ->
            Limit + P2;
        P2 when P2 > Limit ->
            P2 - Limit;
        P2 ->
            P2
    end.

to_list_of_lists(Matrix, N, 1, Acc) ->
    case maps:get({N, 1}, Matrix, undefined) of
        undefined ->
            Acc;
        Value ->
            to_list_of_lists(Matrix, N, 2, [[Value] | Acc])
    end;
to_list_of_lists(Matrix, N, M, [HAcc | TAcc]) ->
    case maps:get({N, M}, Matrix, undefined) of
        undefined ->
            to_list_of_lists(Matrix, N + 1, 1, [HAcc | TAcc]);
        Value ->
            to_list_of_lists(Matrix, N, M + 1, [[Value] ++ HAcc | TAcc])
    end.

print_matrix_aux([], Acc) ->
    Acc ++ "~n";
print_matrix_aux([Row | T], Acc) ->
    NewAcc = lists:foldl(
        fun
            (X, Acc2) when is_list(X) -> 
                X ++ Acc2;
            (X, Acc2) ->
                integer_to_list(X) ++ Acc2
        end,
        Acc,
        Row
    ),
    print_matrix_aux(T, "~n" ++ NewAcc).

print_matrix_aux([], _ValueZero, _ValueElse, Acc) ->
    Acc ++ "~n";
print_matrix_aux([Row | T], ValueZero, ValueElse, Acc) ->
    NewAcc = lists:foldl(
        fun
            ("0", Acc2) -> 
                ValueZero ++ Acc2;
            (0, Acc2) ->
                ValueZero ++ Acc2;
            (X, Acc2) when is_list(X) -> 
                ValueElse ++ Acc2;
            (_X, Acc2) ->
                ValueElse ++ Acc2
        end,
        Acc,
        Row
    ),
    print_matrix_aux(T, ValueZero, ValueElse, "~n" ++ NewAcc).
