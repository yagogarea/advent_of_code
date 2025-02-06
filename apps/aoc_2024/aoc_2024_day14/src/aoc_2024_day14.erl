-module(aoc_2024_day14).

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
    Rows = 103,
    Columns = 101,
    Robots = get_robots(Lines),
    Quadrants = lists:foldl(
        fun({Px, Py, Vx, Vy}, Acc) ->
            {NewPx, NewPy} = aoc_utils_matrices:next_pos({Px, Py, Vx, Vy}, Rows, Columns, 100),
            LimitX = (Rows div 2 + 1),
            LimitY = (Columns div 2 + 1),
            case NewPx == LimitX orelse NewPy == LimitY  of
                false ->
                    case NewPx < LimitX of
                        true ->
                            Quadrantx = top;
                        false ->
                            Quadrantx = bottom
                    end,
                    case NewPy < LimitY of
                        true ->
                            Quadranty = left;
                        false ->
                            Quadranty = right
                    end,
                    add_one({Quadrantx, Quadranty}, Acc);
                true ->
                    Acc
            end
        end,
        #{},
        Robots
    ),
    lists:foldl(fun({_Quadrant, Value}, Acc) -> Value * Acc end, 1, maps:to_list(Quadrants)).


-spec sol2(Input) -> Result when
    Input :: term(),
    Result :: term().
sol2(Input) ->
    Lines = string:lexemes(Input, [$\n]),
    Rows = 103,
    Columns = 101,
    Robots = get_robots(Lines),
    Matrix = lists:foldl(
        fun({Px, Py, _Vx, _Vy}, Acc) ->
            add_one({Px, Py}, Acc)
        end,
        aoc_utils_matrices:matrix(Rows, Columns),
        Robots
    ),
    aoc_utils_matrices:print_matrix(Matrix, ".", "@"),
    move(Matrix, Rows, Columns, Robots, 1, 10000, 1),
    ok.

%%%-----------------------------------------------------------------------------
%%% INTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
add_one(Key, Map) ->
    case maps:get(Key, Map, undefined) of
        undefined ->
            maps:put(Key, 1, Map);
        Number ->
            maps:put(Key, Number + 1, Map)
    end.

get_robots(Lines) ->
    lists:foldl(
        fun(Line, Acc) ->
            [Px, Py, Vx, Vy] = string:lexemes(Line, [$=, $p, $v, 32, $,]),
            Robot = {
                aoc_utils_strings:to_integer(Py) + 1, 
                aoc_utils_strings:to_integer(Px) + 1, 
                aoc_utils_strings:to_integer(Vy),
                aoc_utils_strings:to_integer(Vx) 
            },
            [Robot | Acc]
        end,
        [],
        Lines
    ).

move_one(Pos, NewPos, Map) ->
    ValuePrev = maps:get(Pos, Map),
    NewValue =  maps:get(NewPos, Map),
    NewMap = maps:put(Pos, ValuePrev - 1, maps:put(NewPos, NewValue + 1, Map)),
    NewMap.

move(Matrix, _Rows, _Columns, _PositionAndVelocityList, _Steps, 0, _Acc) ->
    Matrix;
move(Matrix, Rows, Columns, PositionAndVelocityList, Steps, Times, Acc) ->
    {NewMatrix, NewPositions, NewPositionAndVelocityList} = lists:foldl(
        fun({Px, Py, Vx, Vy}, {MatrixAcc, PosAcc, PosisitionsAcc}) ->
            {NewPx, NewPy} = aoc_utils_matrices:next_pos({Px, Py, Vx, Vy}, Rows, Columns, Steps),
            {move_one({Px, Py}, {NewPx, NewPy}, MatrixAcc), [{NewPx, NewPy} | PosAcc], [{NewPx, NewPy, Vx, Vy} | PosisitionsAcc]}
        end,
        {Matrix, [], []},
        PositionAndVelocityList
    ),
    Filtered = lists:filter(
        fun({Px, Py}) ->
            is_not_empty({Px, Py}, NewMatrix) andalso
            is_not_empty({Px + 1, Py - 1}, NewMatrix) andalso 
            is_not_empty({Px + 1, Py + 1}, NewMatrix) andalso
            is_not_empty({Px + 2, Py - 2}, NewMatrix) andalso
            is_not_empty({Px + 2, Py - 2}, NewMatrix) andalso
            is_not_empty({Px + 3, Py - 3}, NewMatrix) andalso
            is_not_empty({Px + 3, Py - 3}, NewMatrix) andalso
            is_not_empty({Px + 4, Py - 4}, NewMatrix) andalso 
            is_not_empty({Px + 4, Py + 4}, NewMatrix) andalso
            is_not_empty({Px + 5, Py - 5}, NewMatrix) andalso
            is_not_empty({Px + 5, Py - 5}, NewMatrix) 
        end,
        NewPositions
    ),
    case Filtered of
        [] ->
            move(NewMatrix, Rows, Columns, NewPositionAndVelocityList, Steps, Times - 1, Acc + 1);
        [{_Px, _Py} | _T] ->
            aoc_utils_matrices:print_matrix(NewMatrix, " ", "@"),
            io:format("~p~n", [Acc]),
            io:read("For write anything ending with a period: ")
    end,
    move(NewMatrix, Rows, Columns, NewPositionAndVelocityList, Steps, Times - 1, Acc + 1).

is_not_empty({Px, Py}, Matrix) ->
    maps:get({Px, Py}, Matrix, 0) > 0.

% move(Matrix, _Rows, _Columns, _PositionAndVelocityList, _Steps, 0) ->
%     Matrix;
% move(Matrix, Rows, Columns, PositionAndVelocityList, Steps, Times) ->
%     {NewMatrix, NewPositionAndVelocityList} = lists:foldl(
%         fun({Px, Py, Vx, Vy}, {MatrixAcc, PosisitionsAcc}) ->
%             {NewPx, NewPy} = aoc_utils_matrices:next_pos({Px, Py, Vx, Vy}, Rows, Columns, Steps),
%             {move_one({Px, Py}, {NewPx, NewPy}, MatrixAcc), [{NewPx, NewPy, Vx, Vy} | PosisitionsAcc]}
%         end,
%         {Matrix, []},
%         PositionAndVelocityList
%     ),
%     aoc_utils_matrices:print_matrix(NewMatrix, ".", "@"),
%     io:format("~p~n", [Times]),
%     move(NewMatrix, Rows, Columns, NewPositionAndVelocityList, Steps, Times - 1).
