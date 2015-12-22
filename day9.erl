-module(day9).
-export([test/0, solve/0]).

% --- Day 9: All in a Single Night ---

% Every year, Santa manages to deliver all of his presents in a single
% night.

% This year, however, he has some new locations to visit; his elves
% have provided him the distances between every pair of locations. He
% can start and end at any two (different) locations he wants, but he
% must visit each location exactly once. What is the shortest distance
% he can travel to achieve this?

% For example, given the following distances:

% London to Dublin = 464
% London to Belfast = 518
% Dublin to Belfast = 141

% The possible routes are therefore:

% Dublin -> London -> Belfast = 982
% London -> Dublin -> Belfast = 605
% London -> Belfast -> Dublin = 659
% Dublin -> Belfast -> London = 659
% Belfast -> Dublin -> London = 605
% Belfast -> London -> Dublin = 982

% The shortest of these is London -> Dublin -> Belfast = 605, and so
% the answer is 605 in this example.

% What is the distance of the shortest route?

input_lines() ->
    { ok, Binary } = file:read_file("input-day9"),
    FileString = binary_to_list(Binary),
    string:tokens(FileString, "\n").

path_string_to_tuple(PathString) ->
    [ From, _, To, _, Cost ] = string:tokens(PathString, " "),
    { From, To, list_to_integer(Cost) }.

input_paths() ->
    [ path_string_to_tuple(PathString) || PathString <- input_lines() ].

places(Paths) ->
    lists:usort([ element(1, X) || X <- Paths ]++[ element(2, X) || X <- Paths ]).

cost(_,_,[]) ->
    impossible;
cost(From, To, [ { From, To, Cost } | _ ] ) ->
    Cost;
cost(From, To, [ { To, From, Cost } | _ ] ) ->
    Cost;
cost(From, To, [ _ | T ]) ->
    cost(From, To, T).

cheapest_path(Paths) ->
    Places = places(Paths),
    lists:min([ cheapest_path(Place, Places -- [Place], Paths) || Place <- Places ]).

cheapest_path(_, [], _) ->
    0;
cheapest_path(CurrentPlace, UnvisitedPlaces, Paths) ->
    lists:min([ cost(CurrentPlace, NewPlace,Paths) + cheapest_path(NewPlace, UnvisitedPlaces -- [NewPlace], Paths) || NewPlace <- UnvisitedPlaces ]).

expensivest_path(Paths)  ->
    Places = places(Paths),
    lists:max([ expensivest_path(Place, Places -- [Place], Paths) || Place <- Places ]).
    
expensivest_path(_, [], _) ->
    0;
expensivest_path(CurrentPlace, UnvisitedPlaces, Paths) ->
    lists:max([ cost(CurrentPlace, NewPlace,Paths) + expensivest_path(NewPlace, UnvisitedPlaces -- [NewPlace], Paths) || NewPlace <- UnvisitedPlaces ]).

    
test() ->   
    TestPaths = [ { "London", "Dublin", 464},
                  { "London", "Belfast", 518},
                  { "Dublin", "Belfast", 141} ],
    605 = cheapest_path(TestPaths),
    982 = expensivest_path(TestPaths),
    ok.

solve() ->
    Paths = input_paths(),
    { cheapest_path(Paths), expensivest_path(Paths) }.
