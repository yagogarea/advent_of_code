-module(aoc_2025_day2).

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
    Lexemes = string:lexemes(Input, [$,,$-,$\n]),
    sol_aux(Lexemes).

-spec sol2(Input) -> Result when
    Input :: term(),
    Result :: term().
sol2(Input) ->
    Lexemes = string:lexemes(Input, [$,,$-,$\n]),
    sol_aux2(Lexemes).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
sol_aux(Lexemes) ->
    sol_aux(Lexemes, "").

sol_aux([], Acc) ->
    lists:foldl(fun (List, FoldAcc) -> list_to_integer(List) + FoldAcc end, 0, Acc);
sol_aux([First, Second | T], Acc) ->
    InvalidIds = find_invalids_in_range(First, Second),
    sol_aux(T,  InvalidIds ++ Acc).

find_invalids_in_range(First, Second) ->
    find_invalids_in_range(First, Second, []).

find_invalids_in_range(Second, Second, Acc) ->
    case is_invalid_id(Second) of
        true ->
           [Second | Acc];
        false ->
            Acc
    end;
find_invalids_in_range(First, Second, Acc) ->
    PlusOne = integer_to_list(list_to_integer(First) + 1),
    case is_invalid_id(First) of
        true ->
            find_invalids_in_range(PlusOne, Second, [First | Acc]);
        false ->
            find_invalids_in_range(PlusOne, Second, Acc)
    end.

is_invalid_id(String) ->
    Length = string:len(String),
    HalfLength = round(Length / 2),
    case Length of
        Len when Len rem 2 == 0  ->
            case {string:slice(String, 0, HalfLength), string:slice(String, HalfLength, HalfLength)} of
                {Same, Same} ->
                    true;
                {_A, _B} ->
                    false
            end;
        _Len ->
            false
    end.

sol_aux2(Lexemes) ->
    sol_aux2(Lexemes, "").

sol_aux2([], Acc) ->
    lists:foldl(fun (List, FoldAcc) -> list_to_integer(List) + FoldAcc end, 0, Acc);
sol_aux2([First, Second | T], Acc) ->
    InvalidIds = find_invalids_in_range2(First, Second),
    sol_aux2(T,  InvalidIds ++ Acc).

find_invalids_in_range2(First, Second) ->
    find_invalids_in_range2(First, Second, []).

find_invalids_in_range2(Second, Second, Acc) ->
    case is_invalid_id2(Second) of
        true ->
           [Second | Acc];
        false ->
            Acc
    end;
find_invalids_in_range2(First, Second, Acc) ->
    PlusOne = integer_to_list(list_to_integer(First) + 1),
    case is_invalid_id2(First) of
        true ->
            find_invalids_in_range2(PlusOne, Second, [First | Acc]);
        false ->
            find_invalids_in_range2(PlusOne, Second, Acc)
    end.

is_invalid_id2(String) ->
    Len = string:length(String),
    is_invalid_id2(String, Len, 1).

is_invalid_id2(_String, Len, Len) ->
    false;
is_invalid_id2(String, Len, Divisor) ->
    Parts = [H | _T] = split_in_same_length(String, Divisor),
    case lists:filter(fun (X) -> X == H end, Parts) of
        [error] ->
            is_invalid_id2(String, Len, Divisor + 1);
        Parts ->
            true;
        _ ->
            is_invalid_id2(String, Len, Divisor + 1)
    end.

split_in_same_length(String, Divisor) ->
    split_in_same_length(String, Divisor, 0, []).

split_in_same_length("", _Divisor, _Pos, Acc) ->
    Acc;
split_in_same_length(String, Divisor, Pos, Acc) ->
    Len = string:len(String),
    case Len < Divisor of
        true ->
            [error];
        false ->
            Part = string:slice(String, Pos, Divisor),
            Rest = string:sub_string(String, 1 + Pos + Divisor),
            split_in_same_length(Rest, Divisor, Pos, [Part | Acc])
    end.
