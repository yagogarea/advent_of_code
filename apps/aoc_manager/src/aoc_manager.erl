-module(aoc_manager).

%%% EXTERNAL EXPORTS
-export([
    read_statement/2,
    init_day/2
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec read_statement(Year, Day) -> Result when
    Year :: integer(),
    Day :: integer(),
    Result :: string() | {error, puzzle_not_found}.
read_statement(Year, Day) ->
    YearString = integer_to_list(Year),
    DayString = integer_to_list(Day),
    request_problem_statement(YearString, DayString).

-spec init_day(Year, Day) -> Result when
    Year :: integer(),
    Day :: integer(),
    Result :: ok | {error, Reason},
    Reason :: puzzle_already_exists | puzzle_not_found.
init_day(Year, Day) ->
    YearString = integer_to_list(Year),
    DayString = integer_to_list(Day),
    ProjectName = "aoc_" ++ YearString ++ "_" ++ "day" ++ DayString,
    Path = "apps/aoc_" ++ YearString  ++ "/" ++ ProjectName ++ "/",
    SourcePath = Path ++ "src/",
    PrivPath = Path ++ "priv/",
    File = SourcePath ++ ProjectName ++ ".erl",
    case filelib:is_file(File) of
        true ->
            {error, puzzle_already_exists};
        false ->
            case request_problem_statement(YearString, DayString) of
                {error, puzzle_not_found} ->
                    {error, puzzle_not_found};
                {Title, Statement} ->
                    filelib:ensure_dir(SourcePath ++ ProjectName),
                    create_app(SourcePath, ProjectName),
                    create_app_src(SourcePath, ProjectName, YearString, DayString),
                    create_priv_files(PrivPath, Title, Statement),
                    create_tests(Path, ProjectName, ProjectName ++ "_tests")
            end
    end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
create_app(SourcePath, ProjectName) ->
    TemplatePath = "templates/aoc_template.erl",
    NewFilePath = SourcePath ++ ProjectName ++ ".erl",
    {ok, Template} = file:read_file(TemplatePath),
    FileContent = binary:replace(Template, <<"aoc_template">>, list_to_binary(ProjectName)),
    {ok, F} = file:open(NewFilePath, [read, write, raw, binary]),
    ok = file:pwrite(F, 0, FileContent),
    ok = file:close(F).

create_app_src(SourcePath, ProjectName, YearString, DayString) ->
    TemplatePath = "templates/aoc_template.app.src",
    NewFilePath = SourcePath ++ ProjectName ++ ".app.src",
    {ok, Template} = file:read_file(TemplatePath),
    ModuleChanged = binary:replace(Template, <<"aoc_template">>, list_to_binary(ProjectName)),
    YearChanged = binary:replace(ModuleChanged, <<"%year">>, list_to_binary(YearString)),
    DayChanged = binary:replace(YearChanged, <<"%day">>, list_to_binary(DayString)),
    {ok, F} = file:open(NewFilePath, [read, write, raw, binary]),
    ok = file:pwrite(F, 0, DayChanged),
    ok = file:close(F).

create_priv_files(PrivPath, _Title, Statement) ->
    filelib:ensure_dir(PrivPath),
    {ok, F} = file:open(PrivPath ++ "statement.txt", [read, write, raw]),
    ok = file:pwrite(F, 0, [Statement]),
    ok = file:close(F).

create_tests(Path, ProjectName, TestName) ->
    filelib:ensure_dir(Path ++ "test/"),
    TemplatePath = "templates/aoc_template_tests.erl",
    NewFilePath = Path ++ "test/" ++ TestName ++ ".erl",
    {ok, Template} = file:read_file(TemplatePath),
    ModuleChanged = binary:replace(Template, <<"aoc_template_tests">>, list_to_binary(TestName)),
    ModuleFunSol = binary:replace(ModuleChanged, <<"aoc_template">>, list_to_binary(ProjectName)),
    ModuleFunSol2 = binary:replace(ModuleFunSol, <<"aoc_template">>, list_to_binary(ProjectName)),
    {ok, F} = file:open(NewFilePath, [read, write, raw, binary]),
    ok = file:pwrite(F, 0, ModuleFunSol2),
    ok = file:close(F).

request_problem_statement(YearString, DayString) ->
    inets:start(),
    Url = aoc_manager_conf:url(),
    Response = httpc:request(get, 
        {
            Url ++ 
            YearString ++
            "/day/" ++
            DayString
        , []
        }, [], []),
    case Response of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
            parse_problem_statement(Body);
        {ok, {{_Version, 404, _ReasonPhrase}, _Headers, _Body}} ->
            {error, puzzle_not_found}
    end.

parse_problem_statement(Body) ->
    [_, Main, _]= re:split(Body, "main>"),
    [_, Title, Rest] = re:split(Main, "---"),
    [Statement, _] = string:split(Rest, "</article>"),
    RemovedTags = re:replace(Statement, "<[^>]+>", "", [global]),
    {Title, RemovedTags}.
