-module(day10).
-export([test/0, solve/0]).
                
see_say(Number) ->
    see_say(Number ,0).

see_say([],_) ->
    [];
see_say([X,X|T], Occurences) ->
    see_say([X|T], Occurences + 1);
see_say([X|T], Occurences) ->
    [ Occurences + 1 + $0, X | see_say(T,0) ].

repeated_see_say(Number, Times) ->
    repeated_see_say(Number, Times, 0).

repeated_see_say(Number, Times, Times) ->
    Number;
repeated_see_say(Number, Times, N) ->
    repeated_see_say(see_say(Number), Times, N + 1).


test() ->
    "11" = see_say("1"),
    "21" = see_say("11"),
    "1211" = see_say("21"),
    "111221" = see_say("1211"),
    "312211" = see_say("111221"),
    ok.

solve() ->
    % Quite slow
    Forty = length(repeated_see_say("1321131112", 40)),
    Fifty = length(repeated_see_say("1321131112", 50)),
    { Forty, Fifty }.
    
