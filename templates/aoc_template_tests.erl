-module(aoc_template_tests).

%%% INCLUDE FILES
-include_lib("eunit/include/eunit.hrl").

%%%-----------------------------------------------------------------------------
%%% TESTS DESCRIPTIONS
%%%-----------------------------------------------------------------------------
sol_test_() ->
    {"Check that the function solves the example case",
        [fun test_sol/0]}.

sol2_test_() ->
    {"Check that the function solves the example case",
        [fun test_sol2/0]}.

%%%-----------------------------------------------------------------------------
%%% TESTS
%%%-----------------------------------------------------------------------------
test_sol() ->
    {Input, Result} = input_result(),
    ?assertEqual(Result, aoc_template:sol(Input)).

test_sol2() ->
    {Input, Result} = input_result2(),
    ?assertEqual(Result, aoc_template:sol2(Input)).

%%%-----------------------------------------------------------------------------
%%% EXAMPLE INPUTS
%%%-----------------------------------------------------------------------------
input_result() ->
    {
        ""
        ,
        ""
    }.

input_result2() ->
    {
        ""
        ,
        ""
    }.
