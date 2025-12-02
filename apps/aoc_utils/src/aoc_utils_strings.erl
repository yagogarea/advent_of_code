-module(aoc_utils_strings).

%%% EXTERNAL EXPORTS
-export([
    to_integer/1,
    split_in_same_length/2
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
to_integer([45 | N]) ->
    Int = list_to_integer(N),
    -Int;
to_integer(N) ->
    list_to_integer(N).

split_in_same_length(String, Divisor) ->
    split_in_same_length(String, Divisor, []).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
split_in_same_length("", _Divisor, Acc) ->
    Acc;
split_in_same_length(String, Divisor, Acc) ->
    Len = string:len(String),
    case Len < Divisor of
        true ->
            error;
        false ->
            Part = string:slice(String, 0, Divisor),
            Rest = string:sub_string(String, 1 + Divisor),
            split_in_same_length(Rest, Divisor, [Part | Acc])
    end.
