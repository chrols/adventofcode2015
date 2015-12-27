-module(day14).
-export([solve/0, test/0]).

convert_reindeer(String) ->
    Words = string:tokens(String," "),
    Speed = list_to_integer(lists:nth(4,Words)),
    Time = list_to_integer(lists:nth(7,Words)),
    Delay = list_to_integer(lists:nth(14,Words)),
    { Speed, Time, Delay }.

input() ->
    { ok, Binary } = file:read_file("input-day14"),
    FileString = binary_to_list(Binary),
    ReindeerStrings = string:tokens(FileString, "\n"),
    [ convert_reindeer(Reindeer) || Reindeer <- ReindeerStrings ].

flight_distance(Reindeer, Time) ->
    { Speed, FlightTime, Delay } = Reindeer,
    flight_distance(Speed, FlightTime, Delay, Time).

flight_distance(Speed, FlightTime, Delay, Time) when Time >= FlightTime+Delay ->
    FlightCycle = FlightTime + Delay,
    Speed*FlightTime * (Time div (FlightCycle)) + flight_distance(Speed, FlightTime, Delay, Time rem FlightCycle);
flight_distance(Speed, FlightTime, _Delay, Time) when Time >= FlightTime ->
    Speed*FlightTime;
flight_distance(Speed, FlightTime, _Delay, Time) when Time =< FlightTime ->
    Speed*Time.

winner(Reindeer, Time, WinnerDistance) ->
    flight_distance(Reindeer,Time) == WinnerDistance.

update_score({ Reindeer, Score }, true) ->
    { Reindeer, Score +1 };
update_score(ScoreEntry, false) ->
    ScoreEntry.

most_points(ReindeerGroup) ->
    Scoring = points_instant([ { Reindeer, 0 } || Reindeer <- ReindeerGroup ],1),
    lists:max([ Score || { _, Score } <- Scoring ]).

points_instant(Scoreboard, 2504) -> 
    Scoreboard;
points_instant(Scoreboard, Time) -> 
    Distances = [ flight_distance(Reindeer, Time) || { Reindeer, _ } <- Scoreboard ],
    Longest = lists:max(Distances),
    points_instant([ update_score({ Reindeer, Score },winner(Reindeer,Time,Longest)) || { Reindeer, Score } <- Scoreboard ], Time + 1).
        
test() ->
    Comet = { 14, 10, 127 },
    Dancer = { 16, 11, 162 },
    1120 = flight_distance(Comet, 1000),
    1056 = flight_distance(Dancer, 1000),
    most_points([Comet, Dancer]),
    ok.

solve(Time) ->
    ReindeerInput = input(),
    First = lists:max([ flight_distance(Reindeer, Time) || Reindeer <- ReindeerInput ]),
    Second = most_points(ReindeerInput),
    { First, Second }.
        
solve() ->
    solve(2503).
