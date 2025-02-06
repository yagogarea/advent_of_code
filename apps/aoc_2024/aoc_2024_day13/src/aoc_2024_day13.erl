-module(aoc_2024_day13).

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
    BinList = re:split(Input, [$\n,$\n]),
    lists:foldl(
        fun(Bin, Acc) ->
            [
                "Button", "A", "X", A, "Y", B,
                "Button", "B", "X", D, "Y", E,
                "Prize", "X", C, "Y", F
            ] = string:lexemes(binary_to_list(Bin), [32, $\n, $:, $+, $=, $,]),
            Machine = 
                {
                    {{list_to_integer(A), list_to_integer(D)}, list_to_integer(C)},
                    {{list_to_integer(B), list_to_integer(E)}, list_to_integer(F)}
                },
            {X, Y} = system_of_equations(Machine),
            RoundedX = round(X),
            RoundedY = round(Y),
            case X == RoundedX andalso Y == RoundedY of
               true -> % Solution
                    io:format("Machine ~p solution ~p~n", [Machine, {X, Y}]),
                    Acc + RoundedX * 3 + RoundedY * 1;
                false ->
                    Acc
            end
        end, 
        0,
        BinList
    ).


-spec sol2(Input) -> Result when
    Input :: term(),
    Result :: term().
sol2(Input) ->
    BinList = re:split(Input, [$\n,$\n]),
    lists:foldl(
        fun(Bin, Acc) ->
            [
                "Button", "A", "X", A, "Y", B,
                "Button", "B", "X", D, "Y", E,
                "Prize", "X", C, "Y", F
            ] = string:lexemes(binary_to_list(Bin), [32, $\n, $:, $+, $=, $,]),
            Machine = 
                {
                    {{list_to_integer(A), list_to_integer(D)}, 10000000000000 + list_to_integer(C)},
                    {{list_to_integer(B), list_to_integer(E)}, 10000000000000 + list_to_integer(F)}
                },
            {X, Y} = system_of_equations(Machine),
            RoundedX = round(X),
            RoundedY = round(Y),
            case X == RoundedX andalso Y == RoundedY of
               true -> 
                    Acc + RoundedX * 3 + RoundedY * 1;
                false ->
                    Acc
            end
        end, 
        0,
        BinList
    ).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
system_of_equations({{{A, B}, C}, {{D, E}, F}}) ->
    Y = (D*C - A*F) / (D*B - A*E),
    X = (E*C - B*F) / (E*A - B*D),
    {X,Y}.
