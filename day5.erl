-module(day5).
-export([test/0, solve/0]).
-compile(export_all).

% --- Day 5: Doesn't He Have Intern-Elves For This? ---

% Santa needs help figuring out which strings in his text file are
% naughty or nice.

% A nice string is one with all of the following properties:

%     It contains at least three vowels (aeiou only), like aei,
%     xazegov, or aeiouaeiouaeiou.

%     It contains at least one letter that appears twice in a row,
%     like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).

%     It does not contain the strings ab, cd, pq, or xy, even if they
%     are part of one of the other requirements.

% For example:

%     ugknbfddgicrmopn is nice because it has at least three vowels
%     (u...i...o...), a double letter (...dd...), and none of the
%     disallowed substrings.

%     aaa is nice because it has at least three vowels and a double
%     letter, even though the letters used by different rules overlap.

%     jchzalrnumimnmhp is naughty because it has no double letter.

%     haegwjzuvuyypxyu is naughty because it contains the string xy.

%     dvszwmarrgswjxmb is naughty because it contains only one vowel.

% How many strings are nice?

% --- Part Two ---

% Realizing the error of his ways, Santa has switched to a better
% model of determining whether a string is naughty or nice. None of
% the old rules apply, as they are all clearly ridiculous.

% Now, a nice string is one with all of the following properties:

%     It contains a pair of any two letters that appears at least
%     twice in the string without overlapping, like xyxy (xy) or
%     aabcdefgaa (aa), but not like aaa (aa, but it overlaps).

%     It contains at least one letter which repeats with exactly one
%     letter between them, like xyx, abcdefeghi (efe), or even aaa.

% For example:

%     qjhvhtzxzqqjkmpb is nice because is has a pair that appears
%     twice (qj) and a letter that repeats with exactly one letter
%     between them (zxz).

%     xxyxx is nice because it has a pair that appears twice and a
%     letter that repeats with one between, even though the letters
%     used by each rule overlap.

%     uurcxstgmygtbstg is naughty because it has a pair (tg) but no
%     repeat with a single letter between them.

%     ieodomkazucvgmuy is naughty because it has a repeating letter
%     with one between (odo), but no pair that appears twice.

% How many strings are nice under these new rules?


forbidden_string([$a, $b|_T]) ->
    naughty;
forbidden_string([$c, $d|_T]) ->
    naughty;
forbidden_string([$p, $q|_T]) ->
    naughty;
forbidden_string([$x, $y|_T]) ->
    naughty;
forbidden_string([]) ->
    nice;
forbidden_string([_|T]) ->
    forbidden_string(T).

double_nice([X, X|_T]) ->
    nice;
double_nice([]) ->
    naughty;
double_nice([_|T]) ->
    double_nice(T).

vowel_count([]) ->
    0;
vowel_count([L|T]) when L =:= $a;
                        L =:= $e;
                        L =:= $i;
                        L =:= $o;
                        L =:= $u ->
    1 + vowel_count(T);
vowel_count([_|T]) ->
    vowel_count(T).

vowel_nice(String) ->
    vowel_nice1(vowel_count(String)).

vowel_nice1(N) when N >= 3 ->
    nice;
vowel_nice1(_) ->
    naughty.

nice_string(String) ->
    Vowel = vowel_nice(String),
    Double = double_nice(String),
    Forbidden = forbidden_string(String),
    not lists:member(naughty, [ Vowel, Double, Forbidden ]).


%     It contains a pair of any two letters that appears at least
%     twice in the string without overlapping, like xyxy (xy) or
%     aabcdefgaa (aa), but not like aaa (aa, but it overlaps).

%     It contains at least one letter which repeats with exactly one
%     letter between them, like xyx, abcdefeghi (efe), or even aaa.

contains_pair(X,Y,[X,Y|T]) ->
    true;
contains_pair(_,_,[]) ->
    false;
contains_pair(X,Y,[_|T]) ->
    contains_pair(X,Y,T).

contains_double_pair([X,Y|T]) ->
    case contains_pair(X,Y,T) of 
        false ->
            contains_double_pair([Y|T]);
        true ->
            nice
    end;
contains_double_pair(_) ->
    naughty.    
    
repeat_with_gap([X,_,X|T]) ->
    nice;
repeat_with_gap([]) ->
    naughty;
repeat_with_gap([_|T]) ->
    repeat_with_gap(T).

new_nice_string(String) ->
    Pair = contains_double_pair(String),
    GapRepeat = repeat_with_gap(String),
    not lists:member(naughty, [ Pair, GapRepeat ]).

test() ->
    true = nice_string("ugknbfddgicrmopn"),
    true = nice_string("aaa"),
    false = nice_string("jchzalrnumimnmhp"),
    false = nice_string("haegwjzuvuyypxyu"),
    false = nice_string("dvszwmarrgswjxmb"),
    true = new_nice_string("qjhvhtzxzqqjkmpb"),
    true = new_nice_string("xxyxx"),
    false = new_nice_string("uurcxstgmygtbstg"),
    false = new_nice_string("ieodomkazucvgmuy"),
    ok.

input_string_list() ->
    { ok, Binary } = file:read_file("input-day5"),
    FileString = binary_to_list(Binary),
    string:tokens(FileString, "\n").

solve() ->
    Input = input_string_list(),
    RegularNice = length(lists:filter(fun nice_string/1, Input)),
    NewNice = length(lists:filter(fun new_nice_string/1, Input)),
    { RegularNice, NewNice }.
