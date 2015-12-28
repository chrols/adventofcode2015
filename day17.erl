-module(day17).
-export([test/0, solve/0]).

input() ->
    [ 33,14,18,20,45,35,16,35,1,13,18,13,50,44,48,6,24,41,30,42 ].

combos(1, L) -> [[X] || X <-L];
combos(K, L) when K == length(L) -> [L];
combos(K, [H|T]) ->
    [[H | Subcombos] || Subcombos <- combos(K-1, T)]
    ++(combos(K, T)).
 
combos(L) ->
    lists:foldl(
        fun(K, Acc) -> Acc++(combos(K, L)) end,
        [[]],
        lists:seq(1, length(L))
    ).

solutions(Containers, Max) ->
    [ L || L <- combos(Containers), lists:sum(L) =:= Max].

smallest_solution(Solutions) ->
    lists:min(lists:map(fun (X) -> length(X) end, Solutions)).

good_solutions(Solutions, Length) ->
    length(lists:filter(fun (X) -> length(X) =:= Length end,
                        Solutions)).                        

test() ->
    Containers = [ 20, 15, 10, 5 ,5 ],
    4 = solutions(Containers, 25),
    ok.

solve() ->
    Input = input(),
    Solutions = solutions(Input, 150),
    First = length(Solutions),    
    Second = good_solutions(Solutions, smallest_solution(Solutions)),
    { First, Second }.
    
