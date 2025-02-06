-module(aoc_2024_day3).

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
    {match, Matches} = re:run(Input, "mul[(][0-9]{1,3},[0-9]{1,3}[)]", [global]),
    sol_aux(Input, Matches).

-spec sol2(Input) -> Result when
    Input :: term(),
    Result :: term().
sol2(Input) ->
    {match, DoMatches} = re:run(Input, "do[(][])]", [global]),
    {match, DontMatches} = re:run(Input, "don't[(][)]", [global]),
    {match, Matches} = re:run(Input, "mul[(][0-9]{1,3},[0-9]{1,3}[)]", [global]),
    Filtered = filter(Matches, DoMatches, DontMatches),
    sol_aux(Input, Filtered).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
sol_aux(String, Matches) ->
    sol_aux(String, Matches, 0).

sol_aux(_String, [], Acc) ->
    Acc;
sol_aux(String, [[{Pos, _Length}] | Rest], Acc) ->
    "mul(" ++ X = string:sub_string(String, Pos + 1),
    [N1, N2 | _Rest] = string:lexemes(X, [$,, $(, $)]),
    sol_aux(String, Rest, Acc + list_to_integer(N1) * list_to_integer(N2)).

filter(Matches, DoMatches, DontMatches) ->
    filter(Matches, DoMatches, DontMatches, []).

filter([], _DoMatches, _DontMatches, Acc) ->
    Acc;
filter([[{Pos, _}] = E | Rest], DoMatches, DontMatches, Acc) ->
    case lists:filter(fun([{X, _}]) -> X < Pos end, DontMatches) of
        [] ->
            filter(Rest, DoMatches, DontMatches, [E | Acc]);
        [{DontPos, _}] ->
            case lists:filter(fun([{X, _}]) -> (X < Pos) andalso (X > DontPos) end, DoMatches) of
                [] ->
                    filter(Rest, DoMatches, DontMatches, Acc);
                _ ->
                    filter(Rest, DoMatches, DontMatches, [E | Acc])
            end;
        DontPosList ->
            DontPos = lists:foldl(
                    fun([{X, _}], Accc) -> 
                        case X > Accc of
                            true ->
                                X;
                            false ->
                                Accc
                        end
                    end,
                    0, DontPosList),
            case lists:filter(fun([{X, _}]) -> (X < Pos) andalso (X > DontPos) end, DoMatches) of
                [] ->
                    filter(Rest, DoMatches, DontMatches, Acc);
                _ ->
                    filter(Rest, DoMatches, DontMatches, [E | Acc])
            end
    end.
