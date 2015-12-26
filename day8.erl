-module(day8).
-export([solve/0]).

string_count([$\\, $x, _, _ | T]) ->
    1 + string_count(T);
string_count([$\\, $\\ |T]) ->
    1 + string_count(T);
string_count([$\\, $"|T]) ->
    1 + string_count(T);
string_count([_|T]) ->
    1 + string_count(T);
string_count([]) ->
    0.

memory_size(String) ->
    string_count(String) - 2.
                
encode([$\\|T]) ->
    2 + encode(T);
encode([$"|T]) ->
    2 + encode(T);
encode([_|T]) ->
    1 + encode(T);
encode([]) ->
    0.

encode_size(String) ->
    encode(String) + 2.

input() ->
    { ok, Binary } = file:read_file("input-day8"),
    FileString = binary_to_list(Binary),
    Strings = string:tokens(FileString, "\n"),
    Strings.

solve() ->
    Input = input(),
    RepresentationSum = lists:sum([ length(String) || String <- Input ]),
    MemorySum = lists:sum([ memory_size(String) || String <- Input ]),
    Encoded = lists:sum([ encode_size(String) || String <- Input ]),
    Saved = RepresentationSum - MemorySum,
    Wasted = Encoded - RepresentationSum,
    { Saved, Wasted }.
