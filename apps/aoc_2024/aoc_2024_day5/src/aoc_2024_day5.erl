-module(aoc_2024_day5).

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
    [Input1, Input2] = re:split(Input, [$\n,$\n]),
    Lines1 = string:lexemes(binary_to_list(Input1), [$\n, 32]),
    Lines2 = string:lexemes(binary_to_list(Input2), [$\n, 32]),
    Rules = lists:foldl(fun(Rule, Acc) -> [string:lexemes(Rule, [$|]) | Acc] end, [], Lines1),
    Chains = lists:foldl(fun(String, Acc) -> [string:lexemes(String, [$,]) | Acc] end, [], Lines2),
    lists:foldl(
        fun(X, Acc)->
            case verify_list(X, Rules) of
                true ->
                    N = lists:nth(round(length(X) / 2), X),
                    list_to_integer(N) + Acc;
                false ->
                    Acc
            end
        end,
        0,
        Chains
    ).

-spec sol2(Input) -> Result when
    Input :: term(),
    Result :: term().
sol2(Input) ->
    [Input1, Input2] = re:split(Input, [$\n,$\n]),
    Lines1 = string:lexemes(binary_to_list(Input1), [$\n, 32]),
    Lines2 = string:lexemes(binary_to_list(Input2), [$\n, 32]),
    Rules = lists:foldl(fun(Rule, Acc) -> [string:lexemes(Rule, [$|]) | Acc] end, [], Lines1),
    Chains = lists:foldl(fun(String, Acc) -> [string:lexemes(String, [$,]) | Acc] end, [], Lines2),
    BadLists = lists:filter(fun(X) -> verify_list(X, Rules) == false end, Chains),
    lists:foldl(
        fun(X, Acc)->
            Fixed = fix(X, Rules),
            N = lists:nth(round(length(Fixed) / 2), Fixed),
            list_to_integer(N) + Acc
        end,
        0,
        BadLists
    ).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
verify_list(String, Rules) ->
    {_, Bool} =lists:foldl(
        fun(Char, {Pos, AccList}) -> 
            FilteredRules = lists:filter(fun(Rule) -> lists:nth(2, Rule) == Char end, Rules),
            case FilteredRules of
                [] ->
                    {Pos + 1, AccList};
                _ -> 
                    Value = lists:foldl(fun([Previous, _Char], Acc) ->
                        case Acc of
                            false ->
                                false;
                            _ ->
                                {_, RestList} = lists:split(Pos, String), 
                                case lists:member(Previous, RestList) of
                                    true ->
                                        false;
                                    false ->
                                        true
                                end
                        end
                    end, true, FilteredRules), 
                    {Pos + 1,  Value andalso AccList}
            end
        end,
        {0, true},
        String
    ),
    Bool.

% verify_list([], _Rules) ->
%     true;
% verify_list([H | T], Rules) ->
%     case verify_list(H, T, Rules) of
%         true ->
%             verify_list(T, Rules);
%         false ->
%             false
%     end.

% verify_list(_H, _T, []) ->
%     true;
% verify_list(H, T, [[X, H] | Rest]) ->
%     case lists:member(X, T) of
%         true ->
%             false;
%         false ->
%             verify_list(H, T, Rest)
%     end;
% verify_list(H, T, [_IrrelevantRule | Rest]) ->
%     verify_list(H, T, Rest).

fix(Acc, Rules) ->
    FilteredRules = lists:filter(
        fun([Prev, Next]) -> 
            lists:member(Prev, Acc) andalso 
            lists:member(Next, Acc)
            end,
        Rules),
    build_list(FilteredRules, []).    

build_list([], Acc) ->
    Acc;
build_list([[Prev, Next]], Acc) ->
    build_list([], [Prev] ++ [Next] ++ Acc);
build_list(Rules, Acc) ->
    Prev = only_in_next(Rules),
    Filters2 = aux(Prev, Rules),
    build_list(Filters2, Prev ++ Acc).
    
only_in_next(Rules) ->
    {Prevs, Nexts} = split(Rules),
    lists:usort(lists:filter(
        fun(Next) -> 
            lists:member(Next, Prevs) == false
            end,
            Nexts)).

aux(List, Rules) ->
    aux(List, Rules, []).

aux(_List, [], Acc) ->
    Acc;
aux(List, [[X, Y] | T], Acc) ->
    case lists:member(Y, List) of
        true ->
            aux(List, T, Acc);
        false ->
            aux(List, T, [[X, Y] | Acc])
    end.

split(List) ->
    lists:foldl(fun([X, Y], {Acc1, Acc2}) -> {[X | Acc1], [Y | Acc2]} end, {[],[]}, List).
