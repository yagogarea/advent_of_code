# Advent of Code 

This repository contains my solutions to the Advent of Code puzzles using Erlang and the `rebar` build tool.

## Overview
To start working on a new puzzle, first, enter the Erlang shell with:

```shell
rebar3 shell
```
Once inside the shell, you can then initialize a new day with:

```erlang
aoc_manager:init_day(Year, Day).
```
This command automatically creates a directory with a template for the specified year and day to develop and test solutions. Additionally, it sends an HTTP request to fetch the HTML content of the puzzle's page for the specified day and extracts the problem statement from that HTML. 

## Running Solutions
To run the solutions, you can use the following command:

```shell
rebar3 shell
```

Then, you can run the solution for part one of a specific day with:
```erlang
aoc_${Year}_day${Day}:sol(${Input}).
```

The solution of the second part of the puzzle is also available with:
```erlang
aoc_${Year}_day${Day}:sol2(${Input}).
```

Replace `${Year}`, `${Day}`, and `${Input}` with the appropriate year, day, and input data for the puzzle. 

## Advent of Code Data
I do not upload the content of the Advent of Code problem statements in this repository as per the creator's guidelines for Advent of Code. Because of this, you will not see the problem statements or the tests in this repository.
