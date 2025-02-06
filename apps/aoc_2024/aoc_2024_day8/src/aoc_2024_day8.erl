-module(aoc_2024_day8).

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
    ets:new(antennas, [bag, named_table, private]),
    Matrix = aoc_utils_matrices:list_to_matrix(string:lexemes(Input, [$\n, 32])),
    MatrixList = maps:to_list(Matrix),
    AntennasTypes = lists:foldl(
        fun({{X, Y}, Value}, Acc) -> 
            case Value == $. of
                true ->
                    Acc;
                false ->
                    ets:insert(antennas, {Value, {X, Y}}),
                    aoc_utils_lists:union([Value], Acc)
            end
        end, 
        [],
        MatrixList
    ), 
    MatrixDimension = aoc_utils_matrices:dimension(Matrix),
    Antinodes = antinodes(MatrixDimension, antennas, AntennasTypes, 1, exclude),
    length(Antinodes).

-spec sol2(Input) -> Result when
    Input :: term(),
    Result :: term().
sol2(Input) ->
    ets:new(antennas2, [bag, named_table, private]),
    Matrix = aoc_utils_matrices:list_to_matrix(string:lexemes(Input, [$\n, 32])),
    MatrixList = maps:to_list(Matrix),
    AntennasTypes = lists:foldl(
        fun({{X, Y}, Value}, Acc) -> 
            case Value == $. of
                true ->
                    Acc;
                false ->
                    ets:insert(antennas2, {Value, {X, Y}}),
                    aoc_utils_lists:union([Value], Acc)
            end
        end, 
        [],
        MatrixList
    ), 
    MatrixDimension = aoc_utils_matrices:dimension(Matrix),
    Antinodes = antinodes(MatrixDimension, antennas2, AntennasTypes, MatrixDimension+20, include),
    length(Antinodes).

antinodes(MatrixDimension, EtsName, AntennasTypes, N, Opt) ->
    antinodes(MatrixDimension, EtsName, AntennasTypes, N, Opt, []).

antinodes(_MatrixDimension, _EtsName, [], _N, _Opt, Acc) ->
    Acc;
antinodes(MatrixDimension, EtsName, [AntennaType | T], N, Opt, Acc) ->
    Antennas = ets:lookup(EtsName, AntennaType),
    Antinodes = distances(MatrixDimension, Antennas, N, Opt),
    Flatten = lists:flatten(Antinodes),
    antinodes(MatrixDimension, EtsName, T, N, Opt, aoc_utils_lists:union(Flatten, Acc)).

distances(MatrixDimension, Antennas, N, Opt) ->
    distances(MatrixDimension, Antennas, N, Opt, []).

distances(_MatrixDimension, [_H], _N, _Opt, Acc) ->
    Acc;
distances(MatrixDimension, [{_Value, {X, Y}} | T], N, Opt, Acc) ->
    NewAcc = lists:foldl(
        fun({_ValueE, {PosX, PosY}}, AccFold) ->
            Antinodes = create_antinodes({X, Y}, {PosX, PosY}, N, Opt),
            ValidAntinodes = valid_elements(MatrixDimension, Antinodes),
            aoc_utils_lists:union(ValidAntinodes, AccFold)
        end,
        Acc,
        T
    ),
    distances(MatrixDimension, T, N, Opt, NewAcc).

create_antinodes(Antenna1, Antenna2, N, Opt) ->
    create_antinodes(Antenna1, Antenna2, N, Opt, []).

create_antinodes(_Antenna1, _Antenna2, -1, _Opt, Acc) ->
    Acc;
create_antinodes(_Antenna1, _Antenna2, 0, exclude, Acc) ->
    Acc;
create_antinodes({X, Y}, {PosX, PosY}, N, Opt, Acc) ->
    Antinode1 = {X - ((PosX - X) * N), Y - ((PosY - Y) * N)},
    Antinode2 = {PosX - ((X - PosX) * N), PosY - ((Y - PosY) * N)},
    create_antinodes({X, Y}, {PosX, PosY}, N - 1, Opt, aoc_utils_lists:union([Antinode1, Antinode2], Acc)).

valid_elements(MatrixDimension, Elements) ->
    valid_elements(MatrixDimension, Elements, []).

valid_elements(_MatrixDimension, [], Acc) ->
    Acc;
valid_elements(MatrixDimension, [H | T], Acc) ->
    case aoc_utils_matrices:is_valid_coods(MatrixDimension, H) of
        true ->
            valid_elements(MatrixDimension, T, aoc_utils_lists:union([H], Acc));
        false ->
            valid_elements(MatrixDimension, T, Acc)
    end.

