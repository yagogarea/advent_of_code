-module(aoc_utils_lists).

%%% EXTERNAL EXPORTS
-export([
    chunks_of/2,
    count/2,
    count_tuple_key/2,
    delete/2,
    find/2,
    find_first/2,
    get/2,
    group/1,
    how_many/2,
    insert/3,
    intersection/2,
    union/2,
    replace/3,
    last/1,
    made_of/2
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
chunks_of(List, Number) ->
    chunks_of(List, Number, 1, lists:duplicate(Number, [])).

count(List, Element) ->
    count(List, Element, 0).
    
count_tuple_key(List, Element) ->
    count_tuple_key(List, Element, 0).

delete(List, Pos) ->
    case Pos > length(List) orelse Pos < 1 of
        true ->
            {error, index_out_of_bounds};
        false ->
            delete(List, Pos - 1, [])
    end.

find(Element, List) ->
    find(Element, List, 1).

find(_Element, [], _Acc) ->
    undefined;
find(Element, [Element | _T], Acc) ->
    Acc;
find(Element, [_H | T], Acc) ->
    find(Element, T, Acc + 1).

find_first(_Condition, []) ->
    [];
find_first(Condition, [H | T]) ->
    case Condition(H) of
        true ->
            H;
        false ->
            find_first(Condition, T)
    end.

get(List, Pos) ->
    case Pos > length(List) orelse Pos < 1 of
        true ->
            {error, index_out_of_bounds};
        false ->
            get_aux(List, Pos - 1)
    end.

group(List) ->
    group(List, []).

how_many(Fun, List) ->
    how_many(Fun, List, 0).

insert(List, Element, Pos) ->
    case Pos > length(List) orelse Pos < 1 of
        true ->
            List ++ [Element];
        false ->
            insert(List, Element, Pos - 1, [])
    end.

intersection(List1, List2) ->
    [X || X <- List1, lists:member(X, List2)].

union(List1, List2) ->
    [X || X <- List1, lists:member(X, List2) == false] ++ List2.

replace(List, Element, Pos) ->
    case Pos > length(List) orelse Pos < 1 of
        true ->
            {error, index_out_of_bounds};
        false ->
            replace(List, Element, Pos - 1, [])
    end.

last([H]) ->
    H;
last([_H | T]) ->
    last(T).

made_of(Element, N) ->
    made_of(Element, N, []).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
chunks_of([], _Number, _ListPos, Acc) ->
    Acc;
chunks_of([H | T], Number, Number, Acc) ->
    NewElement = get(Acc, Number) ++ [H],
    NewAcc = replace(Acc, NewElement, Number),
    chunks_of(T, Number, 1, NewAcc);
chunks_of([H | T], Number, ListPos, Acc) ->
    NewElement = get(Acc, ListPos) ++ [H],
    NewAcc = replace(Acc, NewElement, ListPos),
    chunks_of(T, Number, ListPos + 1, NewAcc).

count([], _Element, Acc) ->
    Acc;
count([Element | T], Element, Acc) ->
    count(T, Element, Acc + 1);
count([_H | T], Element, Acc) ->
    count(T, Element, Acc).

count_tuple_key([], _Key, Acc) ->
    Acc;
count_tuple_key([{Key, _Value} | T], Key, Acc) ->
    count_tuple_key(T, Key, Acc + 1);
count_tuple_key([_H | T], Key, Acc) ->
    count_tuple_key(T, Key, Acc).

get_aux([H | _T], 0) ->
    H;
get_aux([_H | T], Pos) ->
    get_aux(T, Pos - 1).

group([], Acc) ->
    lists:reverse(Acc);
group([H | T], []) ->
    group(T, [[H]]);
group([H | T], [HAcc | TAcc]) ->
    case HAcc of
        [H | _T2] ->
            group(T, [[H] ++ HAcc | TAcc]);
        [_H2 | _T2] ->
            group(T, [[H], HAcc | TAcc])
    end.

delete([], _Pos, Acc) ->
    Acc;
delete([_A | Rest], 0, Acc) ->
    lists:reverse(Acc) ++ Rest;
delete([A | Rest], Pos, Acc) ->
    delete(Rest, Pos - 1, [A |Acc]).

how_many(_Fun, [], Acc) ->
    Acc;
how_many(Fun, [H | Rest], Acc) ->
    case Fun(H) of
        true ->
            how_many(Fun, Rest, Acc + 1);
        false ->
            how_many(Fun, Rest, Acc)
    end.

insert([], _Element, _Pos, Acc) ->
    Acc;
insert(List, Element, 0, Acc) ->
    lists:reverse([Element | Acc]) ++ List;
insert([A | Rest], _Element, Pos, Acc) ->
    insert(Rest, _Element, Pos - 1, [A |Acc]).

replace([], _Element, _Pos, Acc) ->
    Acc;
replace([_A | Rest], Element, 0, Acc) ->
    lists:reverse([Element | Acc]) ++ Rest;
replace([A | Rest], _Element, Pos, Acc) ->
    replace(Rest, _Element, Pos - 1, [A |Acc]).

made_of(_Element, 0, Acc) ->
    Acc;
made_of(Element, N, Acc) ->
    made_of(Element, N - 1, [Element | Acc]).
