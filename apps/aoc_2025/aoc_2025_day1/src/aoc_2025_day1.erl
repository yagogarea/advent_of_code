-module(aoc_2025_day1).

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
    StartPosition = 50,
    Lexemes = string:lexemes(Input, [$\n, 32]),
    sol_aux(StartPosition, Lexemes).

-spec sol2(Input) -> Result when
    Input :: term(),
    Result :: term().
sol2(Input) ->
    StartPosition = 50,
    Lexemes = string:lexemes(Input, [$\n, 32]),
    sol_aux2(StartPosition, Lexemes).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
sol_aux(StartPosition, Instructions) ->
    sol_aux(StartPosition, Instructions, 0).

sol_aux(_Position, [], Acc) ->
    Acc;
sol_aux(Position, ["L" ++ N | T], Acc) ->
    NewPosition = Position - list_to_integer(N),
    apply_rotation(NewPosition, T, Acc);
sol_aux(Position, ["R" ++ N | T], Acc) ->
    NewPosition = Position + list_to_integer(N),
    apply_rotation(NewPosition, T, Acc).

apply_rotation(NewPosition, T, Acc) ->
    case NewPosition rem 100 of
        0 ->
            sol_aux(0, T, Acc + 1);
        Number ->
            sol_aux(Number, T, Acc)
    end.

sol_aux2(StartPosition, Instructions) ->
    sol_aux2(StartPosition, Instructions, 0).

sol_aux2(_Position, [], Acc) ->
    Acc;
sol_aux2(Position, ["L" ++ N | T], Acc) ->
    NewPosition = Position - list_to_integer(N),
    apply_rotation2(Position, list_to_integer(N), NewPosition, T, Acc);
sol_aux2(Position, ["R" ++ N | T], Acc) ->
    NewPosition = Position + list_to_integer(N),
    apply_rotation2(Position, list_to_integer(N), NewPosition, T, Acc).

apply_rotation2(OldPosition, _Increment, NewPosition, T, Acc) ->
    case NewPosition of
        0 ->
            case OldPosition of
                0 ->
                    sol_aux2(0, T, Acc);
                _Other ->
                    sol_aux2(0, T, Acc + 1)
            end;
        100 ->
            sol_aux2(0, T, Acc + 1);
        Number when Number < 0 ->
            case OldPosition of
                0 ->
                    sol_aux2((Number rem 100 + 100) rem 100, T, Acc + NewPosition div -100);
                _Other ->
                    sol_aux2((Number rem 100 + 100) rem 100, T, Acc + 1 + NewPosition div -100)
            end;
        Number when Number > 100 ->
            sol_aux2(Number rem 100, T, Acc + NewPosition div 100);
        Number ->
            sol_aux2(Number, T, Acc)
    end.
