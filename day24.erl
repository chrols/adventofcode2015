-module(day24).
-export([test/0, solve/0]).

solutions([], Group, TargetWeight) ->
    GroupSum = lists:sum(Group),
    if GroupSum == TargetWeight ->
            [Group];
       true ->
            []
    end;
solutions([Present|T], Group, TargetWeight) ->
    GroupSum = lists:sum(Group),
    if GroupSum > TargetWeight ->
            [];
       GroupSum == TargetWeight ->
            [Group];
       GroupSum < TargetWeight ->
            solutions(T, [Present|Group], TargetWeight) ++
            solutions(T, Group, TargetWeight)
    end.

product([]) ->
    1;
product([N|T]) ->
    N * product(T).

solution_comparison(S1, S2) ->
    if length(S1) < length(S2) -> true;
       length(S1) > length(S2) -> false;
       length(S1) == length(S2) -> product(S1) < product(S2)
    end.    

sort_solutions(Solutions) ->
    lists:usort(fun solution_comparison/2, Solutions).
                   
test() ->
    Presents = lists:seq(1,5) ++ lists:seq(7,11),
    99 = solve(Presents,3),
    ok.

input() ->
    { ok, Binary } = file:read_file("input-day24"),
    FileString = binary_to_list(Binary),
    PresentStrings = string:tokens(FileString, "\n"),
    [ list_to_integer(Present) || Present <- PresentStrings ].

solve(Presents, Compartments) ->
    TargetWeight = lists:sum(Presents) div Compartments,
    Solutions = solutions(Presents, [], TargetWeight),
    product(hd(sort_solutions(Solutions))).

solve() ->
    Presents = input(),
    FirstSolution = solve(Presents,3),
    TrunkSolution = solve(Presents,4),
    { FirstSolution, TrunkSolution }.
