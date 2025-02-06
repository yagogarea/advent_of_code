-module(aoc_2024_day9).

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
    IntegerList = [X || X <- Input],
    {BlocksAndSpaces, _Id, _Spacebool} = lists:foldl(
            fun(X, {List, ID, SpaceBool}) ->
                case SpaceBool of
                    true ->
                        {lists:duplicate(list_to_integer([X]), [$.]) ++ List, ID, false};
                    false ->
                        {lists:duplicate(list_to_integer([X]), ID) ++ List, ID + 1, true}
                end
            end, 
            {[], 0, false}, 
            IntegerList
        ),
    CompactedList = compact(lists:reverse(BlocksAndSpaces)),
    {Checksum, _LastPos}= lists:foldl(fun(X, {Acc, Pos}) -> {X * Pos + Acc, Pos + 1} end, {0,0}, CompactedList),
    Checksum.

-spec sol2(Input) -> Result when
    Input :: term(),
    Result :: term().
sol2(Input) ->
    IntegerList = [X || X <- Input],
    {BlocksAndSpaces, _Id, _Spacebool} = lists:foldl(
            fun(X, {List, ID, SpaceBool}) ->
                case SpaceBool of
                    true ->
                        {[lists:duplicate(list_to_integer([X]), [space]) | List], ID, false};
                    false ->
                        {[lists:duplicate(list_to_integer([X]), ID) | List], ID + 1, true}
                end
            end, 
            {[], 0, false}, 
            IntegerList
        ),
    CompactedList = compact_if_possible(lists:reverse(BlocksAndSpaces), [X || X <- BlocksAndSpaces, lists:member([space], X) == false]),
    {Checksum, _LastPos}= lists:foldl(
        fun(X, {Acc, Pos}) -> 
            case X of
                space ->
                    {Acc, Pos + 1};
                _Number ->
                    {X * Pos + Acc, Pos + 1}
            end
        end, {0,0}, lists:flatten(CompactedList)),
    Checksum.


%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTION
%%%-----------------------------------------------------------------------------
compact_if_possible(Blocks, []) ->
    Blocks;
compact_if_possible(Blocks, [H | T]) ->
    case find_before_element(H, Blocks) of
        element_not_in_list ->
            Blocks;
        not_found ->
            compact_if_possible(Blocks, T);
        {Space, Pos} ->
            LengthH = length(H),
            PosH = aoc_utils_lists:find(H, Blocks),
            Deleted = aoc_utils_lists:replace(Blocks, lists:duplicate(LengthH, [space]), PosH),
            Inserted = aoc_utils_lists:replace(Deleted, H, Pos),
            NewBlocks = aoc_utils_lists:insert(Inserted, lists:duplicate(length(Space) - LengthH, [space]), Pos + 1),
            compact_if_possible(NewBlocks, T)
    end.

find_before_element(Element, List) ->
    find_before_element(Element, length(Element), List, 1).

find_before_element(_Element, _Length, [], _Pos) ->
    element_not_in_list;
find_before_element(Element, Length, [[[space] | T2] | T], Pos) ->
    case length([[space] | T2]) >= Length of
        true ->
            {[[space] | T2], Pos};
        false ->
            find_before_element(Element, Length, T, Pos + 1)
    end;
find_before_element(Element, _Length, [Element | _T], _Pos) ->
    not_found;
find_before_element(Element, Length, [_H | T], Pos) ->
    find_before_element(Element, Length, T, Pos + 1).

compact(Blocks) ->
    compact(Blocks, []).

compact([], Acc) ->
    lists:reverse(Acc);
compact(["." | T], Acc) ->
    case length(lists:usort(["." | T])) of
        1 ->
            lists:reverse(Acc);
        _MoreThanDots ->
            {Last, ListWithoutLast} = get_and_drop_last(T),
            case Last of
              "." ->
                    compact(["." | ListWithoutLast], Acc);
                _ ->
                    compact(ListWithoutLast, [Last | Acc])
            end
    end;
compact([Number | T], Acc) ->
    compact(T, [Number | Acc]).

get_and_drop_last(List) ->
    get_and_drop_last(List, []).

get_and_drop_last([H], Acc) ->
    {H, lists:reverse(Acc)};
get_and_drop_last([H | T], Acc) ->
    get_and_drop_last(T, [H | Acc]).
