-module(aoc_manager_conf).

%%% EXTERNAL EXPORTS
-export([
    url/0
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec url() -> Result when
    Result :: string().
url() ->
    application:get_env(advent_manager, url, "https://adventofcode.com/").
