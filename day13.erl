-module(day13).
-compile(export_all).

input() ->
    { ok, Binary } = file:read_file("input-day13"),
    FileString = binary_to_list(Binary),
    [ happiness_convert(String) || String <- string:tokens(FileString, "\n")].

happiness_gain("gain", Number) ->
    list_to_integer(Number);
happiness_gain("lose", Number) ->
    - list_to_integer(Number).

happiness_convert(HappyString) ->
    Words = string:tokens(HappyString,". "),
    Name = lists:nth(1, Words),
    Happiness = happiness_gain(lists:nth(3,Words),
                               lists:nth(4,Words)),
    Neighbour = lists:nth(11,Words),
    { { Name, Neighbour }, Happiness }.


total_happiness(Seating, Prefrence) ->
    ReverseSeating = lists:reverse(Seating),
    First = hd(Seating),
    ReverseFirst = hd(ReverseSeating),
    half_happiness(Seating, Prefrence,First) + half_happiness(ReverseSeating, Prefrence,ReverseFirst).

half_happiness([ P1, P2 | T ], Prefrence, First) ->
    proplists:get_value({P1, P2}, Prefrence) + half_happiness([P2|T], Prefrence, First);
half_happiness([P], Prefrence, First) ->
    proplists:get_value({P, First}, Prefrence).

permutations([]) ->
    [[]];
permutations(L) ->
    [ [H|T] || H <- L,
               T <- permutations(L -- [H]) ].
    
test_input() ->
    TestStrings = 
        [ "Alice would gain 54 happiness units by sitting next to Bob.",
          "Alice would lose 79 happiness units by sitting next to Carol.",
          "Alice would lose 2 happiness units by sitting next to David.",
          "Bob would gain 83 happiness units by sitting next to Alice.",
          "Bob would lose 7 happiness units by sitting next to Carol.",
          "Bob would lose 63 happiness units by sitting next to David.",
          "Carol would lose 62 happiness units by sitting next to Alice.",
          "Carol would gain 60 happiness units by sitting next to Bob.",
          "Carol would gain 55 happiness units by sitting next to David.",
          "David would gain 46 happiness units by sitting next to Alice.",
          "David would lose 7 happiness units by sitting next to Bob.",
          "David would gain 41 happiness units by sitting next to Carol."],
    [ happiness_convert(String) || String <- TestStrings ].
    
test() ->
    Prefrence = test_input(),
    Seating = [ "David", "Alice", "Bob", "Carol" ],
    total_happiness(Seating, Prefrence).

add_you(Prefrence) ->
    People = proplists:get_keys(proplists:get_keys(Prefrence)),
    YourPrefrence = [ { { "You", Neighbour }, 0 } || Neighbour <- People ],
    TheirPrefrence = [ { { Neighbour, "You" }, 0 } || Neighbour <- People ],
    Prefrence ++ YourPrefrence ++ TheirPrefrence.

solve(Prefrence) ->
    People = proplists:get_keys(proplists:get_keys(Prefrence)),
    SeatingPermutations = permutations(People),
    lists:max([ total_happiness(Seating, Prefrence) || Seating <- SeatingPermutations ]).

solve() ->
    Prefrence = input(),
    First = solve(Prefrence),    
    YouPrefrence = add_you(Prefrence),
    Second = solve(YouPrefrence),
    { First, Second }.
