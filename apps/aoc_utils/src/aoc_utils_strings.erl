-module(aoc_utils_strings).

%%% EXTERNAL EXPORTS
-export([
    to_integer/1
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
to_integer([45 | N]) ->
    Int = list_to_integer(N),
    -Int;
to_integer(N) ->
    list_to_integer(N).
