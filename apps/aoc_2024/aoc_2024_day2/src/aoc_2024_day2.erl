-module(aoc_2024_day2).

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
    lists:foldl(
        fun(X, Sum) ->
            StringNumbers = string:lexemes(X, [32]),
            Numbers = lists:map(
                    fun erlang:list_to_integer/1, 
                StringNumbers
            ),
            sol_aux(Numbers) + Sum
        end,
        0,
        Lines
    ).

-spec sol2(Input) -> Result when
    Input :: term(),
    Result :: term().
sol2(Input) ->
    Lines = string:lexemes(Input, [$\n]),
    lists:foldl(
        fun(X, Sum) ->
            StringNumbers = string:lexemes(X, [32]),
            Numbers = lists:map(
                fun(Y) ->
                    list_to_integer(Y)
                end, 
                StringNumbers
            ),
            sol2_aux(Numbers) + Sum
        end,
        0,
        Lines
    ).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
sol_aux(Numbers) ->
    sol_aux(Numbers, undefined).

sol_aux([_A], _Mode) ->
    1;
sol_aux([A, B | Rest], Mode) when (Mode =:= increasing) ->
    case (B - A >= 1) andalso (B - A =< 3) of
        true ->
            sol_aux([B | Rest], Mode);
        false ->
            0
    end;
sol_aux([A, B | Rest], Mode) when (Mode =:= decreasing) ->
    case (A - B >= 1) andalso (A - B =< 3) of
        true ->
            sol_aux([B | Rest], Mode);
        false ->
            0
    end;
sol_aux([A, B | Rest], _Mode)  ->
    case (A - B >= 1) andalso (A - B =< 3) of
        true ->
            sol_aux([B | Rest], decreasing);
        false ->
            case (B - A >= 1) andalso (B - A =< 3) of
                true ->
                    sol_aux([B | Rest], increasing);
                false ->
                    0
            end
    end.

sol2_aux(Numbers) ->
    case sol_aux(Numbers) of
        1 ->
            1;
        0 ->
            sol2_aux(Numbers, 1)
    end.

sol2_aux(Numbers, Pos) ->
    case Pos == length(Numbers) + 1 of
        true ->
            0;
        _ ->
            List = aoc_utils_lists:delete(Numbers, Pos),
            case sol_aux(List) of
                1 ->
                    1;
                0 ->
                    sol2_aux(Numbers, Pos + 1)
            end
    end.
