-module(day3).
-export([test/0, solve/0]).

% --- Day 3: Perfectly Spherical Houses in a Vacuum ---

% Santa is delivering presents to an infinite two-dimensional grid of houses.

% He begins by delivering a present to the house at his starting
% location, and then an elf at the North Pole calls him via radio and
% tells him where to move next. Moves are always exactly one house to
% the north (^), south (v), east (>), or west (<). After each move, he
% delivers another present to the house at his new location.

% However, the elf back at the north pole has had a little too much
% eggnog, and so his directions are a little off, and Santa ends up
% visiting some houses more than once. How many houses receive at
% least one present?

% For example:

%     > delivers presents to 2 houses: one at the starting location,
%     and one to the east.
%     ^>v< delivers presents to 4 houses in a square, including twice
%     to the house at his starting/ending location.
%     ^v^v^v^v^v delivers a bunch of presents to some very lucky
%     children at only 2 houses.

% --- Part Two ---

% The next year, to speed up the process, Santa creates a robot
% version of himself, Robo-Santa, to deliver presents with him.

% Santa and Robo-Santa start at the same location (delivering two
% presents to the same starting house), then take turns moving based
% on instructions from the elf, who is eggnoggedly reading from the
% same script as the previous year.

% This year, how many houses receive at least one present?

% For example:

%     ^v delivers presents to 3 houses, because Santa goes north, and
%     then Robo-Santa goes south.
%     ^>v< now delivers presents to 3 houses, and Santa and Robo-Santa
%     end up back where they started.
%     ^v^v^v^v^v now delivers presents to 11 houses, with Santa going
%     one direction and Robo-Santa going the other.

input() ->
    { ok, Binary } = file:read_file("input-day3"),
    binary_to_list(Binary).

% < => 60
% > => 62
% ^ => 94
% v => 118

next_step(60, {X,Y}) -> {X-1,Y};
next_step(62, {X,Y}) -> {X+1,Y};
next_step(94, {X,Y}) -> {X,Y+1};
next_step(118, {X,Y}) -> {X,Y-1}.
    
traverse(Instructions) ->
    traverse(Instructions, {0,0}, []).

traverse([], Position, Visited) ->
    lists:usort([Position | Visited ]);
traverse([Instruction|T], Position, Visited ) ->
    traverse(T, next_step(Instruction, Position), [ Position | Visited ]).
   
santa_run(StringInput) ->
    length(traverse(StringInput)).

robo_run(StringInput) ->    
    SantaInstructions = [ lists:nth(X,StringInput) || X <- lists:seq(1,length(StringInput),2) ],
    RoboSantaInstructions = [ lists:nth(X,StringInput) || X <- lists:seq(2,length(StringInput),2) ],
    VisitedHouses = traverse(SantaInstructions) ++ traverse(RoboSantaInstructions),
    UniqueHouses = lists:usort(VisitedHouses),
    length(UniqueHouses).

test() ->
    2 = santa_run(">"),
    4 = santa_run("^>v<"),
    2 = santa_run("^v^v^v^v^v"),
    3 = robo_run("^v"),
    3 = robo_run("^>v<"),
    11 = robo_run("^v^v^v^v^v"),
    ok.

solve() ->
    NumRegularRun = santa_run(input()),
    NumRoboRun = robo_run(input()),
    { NumRegularRun, NumRoboRun }.

