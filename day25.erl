-module(day25).
-compile(export_all).

next_value(X) ->
    (X * 252533) rem 33554393.

value(Max) ->
    value(20151125, 1, Max).

value(Value, N, N) ->
    Value;
value(Value, N, Max) ->
    value(next_value(Value), N + 1, Max).

level(1) ->
    1;
level(N) ->
    N + level(N-1).

pos_to_level(X,Y) ->
    X+Y-1.

pos_to_n(X,Y) ->
    level(pos_to_level(X,Y)) - Y + 1.

% X: 1  2  3  4  5
% Y: 5  4  3  2  1
% N: 11 12 13 14 15

test() ->
    31916031 = next_value(20151125),
    11 = pos_to_n(1,5),
    8 = pos_to_n(2,3),
    18 = pos_to_n(3,4),
    27995004 = solve(6,6),
    21345942 = solve(3,4),
    ok.

solve(X,Y) ->
    value(pos_to_n(X,Y)).

solve() ->
    % Enter the code at row 3010, column 3019.
    solve(3019,3010).
