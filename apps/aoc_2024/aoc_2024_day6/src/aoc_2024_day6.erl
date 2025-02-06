-module(aoc_2024_day6).

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
    [{GuardPos, Simbol}] = lists:filter(fun({{_X, _Y}, Value}) -> Value =/= $# andalso Value =/= $. end , MatrixList),
    GuardDirection = guard_dir(Simbol),
    MatrixDimension = aoc_utils_matrices:dimension(Matrix),
    Obstructions = lists:filter(fun({{_X, _Y}, Value}) -> Value == $# end, MatrixList),
    Movements = movements(MatrixDimension, GuardPos, GuardDirection, Obstructions),
    length(Movements).

-spec sol2(Input) -> Result when
    Input :: term(),
    Result :: term().
sol2(Input) ->
    Matrix = aoc_utils_matrices:list_to_matrix(string:lexemes(Input, [$\n, 32])),
    MatrixList = maps:to_list(Matrix),
    [{GuardPos, Simbol}] = lists:filter(fun({{_X, _Y}, Value}) -> Value =/= $# andalso Value =/= $. end , MatrixList),
    GuardDirection = guard_dir(Simbol),
    Obstructions = lists:filter(fun({{_X, _Y}, Value}) -> Value == $# end, MatrixList),
    MatrixDimension = aoc_utils_matrices:dimension(Matrix),
    Movements = movements(MatrixDimension, GuardPos, GuardDirection, Obstructions),
    Positions = obstructions_infinite_loop(GuardPos, GuardDirection, Movements, Obstructions),
    length(Positions).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
movements(MatrixDimension, GuardPos = {GuardX, GuardY}, GuardDirection, Obstructions) ->
    movements(MatrixDimension, GuardPos = {GuardX, GuardY}, GuardDirection, Obstructions, []).

movements(MatrixDimension, GuardPos = {GuardX, GuardY}, GuardDirection, Obstructions, Acc) ->
    {X, Y} = find_first_obstruction(GuardPos, GuardDirection, Obstructions, {0,0}),
    {DirX, DirY} = GuardDirection,
    case {X, Y} of
        {0, 0} -> % no obstruction
            case GuardDirection of
                {-1, 0} ->
                    NewPos = 1,
                    Positions = [{E, GuardY} || E <- interval(GuardX, NewPos, 1)];
                {1, 0} ->
                    NewPos = MatrixDimension,
                    Positions = [{E, GuardY} || E <- interval(GuardX, NewPos, 1)];
                {0, 1} ->
                    NewPos = MatrixDimension,
                    Positions = [{GuardX, E} || E <- interval(GuardY, NewPos, 1)];
                {0, -1} ->
                    NewPos = 1,
                    Positions = [{GuardX, E} || E <- interval(GuardY, NewPos, 1)]
            end,
            aoc_utils_lists:union(Positions, Acc);
        {_X, GuardY} ->
            NewPos = X - DirX,
            Positions = [{E, GuardY} || E <- interval(GuardX, NewPos, 1)],
            NewAcc = aoc_utils_lists:union(Positions, Acc),
            movements(MatrixDimension, {NewPos, GuardY}, rotate_90(GuardDirection), Obstructions, NewAcc);
        {GuardX, _Y} ->
            NewPos = Y - DirY,
            Positions = [{GuardX, E} || E <- interval(GuardY, NewPos, 1)],
            NewAcc = aoc_utils_lists:union(Positions, Acc),
            movements(MatrixDimension, {GuardX, NewPos}, rotate_90(GuardDirection), Obstructions, NewAcc)
    end.

obstructions_infinite_loop(GuardPos, GuardDirection, Movements, Obstructions) ->
    obstructions_infinite_loop(GuardPos, GuardDirection, Movements, Obstructions, []).

obstructions_infinite_loop(_GuardPos, _GuardDirection, [], _Obstructions, Acc) ->
    Acc;
obstructions_infinite_loop(GuardPos, GuardDirection, [H | T], Obstructions, Acc) ->
    Result = try_movement(GuardPos, GuardDirection, [{H, $#} | Obstructions], []),
    case Result of
        false ->
            obstructions_infinite_loop(GuardPos, GuardDirection, T, Obstructions, Acc);
        loop ->
            obstructions_infinite_loop(GuardPos, GuardDirection, T, Obstructions, [H |Acc])
    end.

try_movement(GuardPos = {GuardX, GuardY}, GuardDirection, Obstructions, Acc) ->
    {X, Y} = find_first_obstruction(GuardPos, GuardDirection, Obstructions, {0,0}),
    {DirX, DirY} = GuardDirection,
    case {X, Y} of
        {0, 0} -> % no obstruction
            false;
        {_X, GuardY} ->
            NewPos = X - DirX,
            Positions = [{{E, GuardY}, GuardDirection} || E <- interval(GuardX, NewPos, 1)],
            case aoc_utils_lists:intersection(Positions, Acc) of
                [] ->
                    NewAcc = aoc_utils_lists:union(Positions, Acc),
                    try_movement({NewPos, GuardY}, rotate_90(GuardDirection), Obstructions, NewAcc);
                [_H] ->
                    NewAcc = aoc_utils_lists:union(Positions, Acc),
                    try_movement({NewPos, GuardY}, rotate_90(GuardDirection), Obstructions, NewAcc);
                _NonEmptyList ->
                    loop
            end;
        {GuardX, _Y} ->
            NewPos = Y - DirY,
            Positions = [{{GuardX, E}, GuardDirection} || E <- interval(GuardY, NewPos, 1)],
            case aoc_utils_lists:intersection(Positions, Acc) of
                [] ->
                    NewAcc = aoc_utils_lists:union(Positions, Acc),
                    try_movement({GuardX, NewPos}, rotate_90(GuardDirection), Obstructions, NewAcc);
                [_H] ->
                    NewAcc = aoc_utils_lists:union(Positions, Acc),
                    try_movement({GuardX, NewPos}, rotate_90(GuardDirection), Obstructions, NewAcc);
                _NonEmptyList ->
                    loop
            end
    end.

find_first_obstruction(_GuardPos, _Dir, [], Acc) ->
    Acc;
find_first_obstruction({GuardX, GuardY}, {0, -1}, [{{GuardX, Y}, _Value} | T], {_AccX, AccY}) when GuardY >= Y ->
    case AccY of
        0 ->
            find_first_obstruction({GuardX, GuardY}, {0, -1}, T, {GuardX, Y});
        AccY when Y > AccY ->
            find_first_obstruction({GuardX, GuardY}, {0, -1}, T, {GuardX, Y});
        _AccY ->
            find_first_obstruction({GuardX, GuardY}, {0, -1}, T, {GuardX, AccY})
    end;
find_first_obstruction({GuardX, GuardY}, {0, 1}, [{{GuardX, Y}, _Value} | T], {_AccX, AccY}) when GuardY =< Y ->
    case AccY of
        0 ->
            find_first_obstruction({GuardX, GuardY}, {0, 1}, T, {GuardX, Y});
        AccY when Y < AccY ->
            find_first_obstruction({GuardX, GuardY}, {0, 1}, T, {GuardX, Y});
        _AccY ->
            find_first_obstruction({GuardX, GuardY}, {0, 1}, T, {GuardX, AccY})
    end;
find_first_obstruction({GuardX, GuardY}, {1, 0}, [{{X, GuardY}, _Value} | T], {AccX, _AccY}) when GuardX =< X ->
    case AccX of
        0 ->
            find_first_obstruction({GuardX, GuardY}, {1, 0}, T, {X, GuardY});
        AccX when X < AccX ->
            find_first_obstruction({GuardX, GuardY}, {1, 0}, T, {X, GuardY});
        _AccX ->
            find_first_obstruction({GuardX, GuardY}, {1, 0}, T, {AccX, GuardY})
    end;
find_first_obstruction({GuardX, GuardY}, {-1, 0}, [{{X, GuardY}, _Value} | T], {AccX, _AccY}) when GuardX >= X ->
    case AccX of
        0 ->
            find_first_obstruction({GuardX, GuardY}, {-1, 0}, T, {X, GuardY});
        AccX when X > AccX ->
            find_first_obstruction({GuardX, GuardY}, {-1, 0}, T, {X, GuardY});
        _AccX ->
            find_first_obstruction({GuardX, GuardY}, {-1, 0}, T, {AccX, GuardY})
    end;
find_first_obstruction(GuardPos, GuarDir, [_H | T], Acc) ->
    find_first_obstruction(GuardPos, GuarDir, T, Acc).


interval(A, B, Incr) ->
    case A < B of
        true ->
            lists:seq(A, B, Incr);
        false ->
            lists:seq(B, A, Incr)
    end.


guard_dir($^) ->
    {-1, 0};
guard_dir($v) ->
    {1, 0};
guard_dir($>) ->
    {0, 1};
guard_dir($<) ->
    {0, -1}.

rotate_90({-1, 0}) ->
    {0, 1};
rotate_90({1, 0}) ->
    {0, -1};
rotate_90({0, 1}) ->
    {1, 0};
rotate_90({0, -1}) ->
    {-1, 0}.
