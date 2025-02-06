-module(aoc_puzzle).

%%% EXTERNAL EXPORTS
-export([
    info/2
]).

%%%-----------------------------------------------------------------------------
%%% CALLBACKS
%%%-----------------------------------------------------------------------------
-callback sol(Input) -> Result when
    Input :: term(),
    Result :: term().

-callback sol2(Input) -> Result when
    Input :: term(),
    Result :: term().

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec info(Year, Day) -> Result when
    Year :: integer(),
    Day :: integer(), 
    Result :: string() | {error, puzzle_not_found}.
info(Year, Day) ->
    aoc_manager:read_statement(Year, Day).
