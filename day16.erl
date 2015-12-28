-module(day16).
-export([test/0, solve/0]).
-compile(export_all).


input() ->
    { ok, Binary } = file:read_file("input-day16"),
    FileString = binary_to_list(Binary),
    string:tokens(FileString, "\n").

convert_string(String) ->
    Strings = string:tokens(String, ":, "),
    SueNumber = lists:nth(2, Strings),
    Fact1 = { lists:nth(3,Strings), list_to_integer(lists:nth(4,Strings)) },
    Fact2 = { lists:nth(5,Strings), list_to_integer(lists:nth(6,Strings)) },
    Fact3 = { lists:nth(7,Strings), list_to_integer(lists:nth(8,Strings)) },
    { SueNumber, [ Fact1, Fact2, Fact3] }.
        
check_fact({ X, Y}, [{X, Y}|_]) ->
    consistent;
check_fact({ X, _}, [{X, _}|_]) ->
    inconsistent;
check_fact(Fact, [_|T]) ->
    check_fact(Fact,T);
check_fact(_Fact, []) ->
    no_info.

check_fact2({"cats", Y}, [{"cats", Z} | _]) when Y > Z ->
    consistent;
check_fact2({"cats", Y}, [{"cats", Z} | _]) ->
    inconsistent;
check_fact2({"trees", Y}, [{"trees", Z} | _]) when Y > Z ->
    consistent;
check_fact2({"trees", Y}, [{"trees", Z} | _]) ->
    inconsistent;
check_fact2({"pomeranians", Y}, [{"pomeranians", Z} | _]) when Y < Z ->
    consistent;
check_fact2({"pomeranians", Y}, [{"pomeranians", Z} | _]) ->
    inconsistent;
check_fact2({"goldfish", Y}, [{"goldfish", Z} | _]) when Y < Z ->
    { Y, Z};
check_fact2({"goldfish", Y}, [{"goldfish", Z} | _]) ->
    inconsistent;
check_fact2({ X, Y}, [{X, Y}|_]) ->
    consistent;
check_fact2({ X, _}, [{X, _}|_]) ->
    inconsistent;
check_fact2(Fact, [_|T]) ->
    check_fact2(Fact,T);
check_fact2(_Fact, []) ->
    no_info.

check_facts(Facts, KnownFacts) ->
    [ check_fact(Fact, KnownFacts) || Fact <- Facts ].

check_facts2(Facts, KnownFacts) ->
    [ check_fact2(Fact, KnownFacts) || Fact <- Facts ].

test() ->
    Fact1 = {"pomerians","2"},
    Fact2 = {"pomerians","3"},
    Fact3 = {"trees","2"},
    no_info = check_fact(Fact1, [Fact3, Fact3]),
    consistent = check_fact(Fact1, [Fact3, Fact1]),
    inconsistent = check_fact(Fact1, [Fact3, Fact2]),
    [ no_info, consistent, inconsistent ] = check_facts([Fact3, Fact2, Fact1],[Fact2]),
    ok.

known_facts() ->
    [{ "children",3 },
     { "cats",7 },
     { "samoyeds",2 },
     { "pomeranians",3 },
     { "akitas",0 },
     { "vizslas",0 },
     { "goldfish",5 },
     { "trees",3 },
     { "cars",2 },
     { "perfumes",1 }].

check_sues(SueInfo, KnownFacts) ->
    [ { SueNumber, check_facts(Facts, KnownFacts) } || { SueNumber, Facts } <- SueInfo ].

check_sues2(SueInfo, KnownFacts) ->
    [ { SueNumber, check_facts2(Facts, KnownFacts) } || { SueNumber, Facts } <- SueInfo ].

consistent_sue(SueConsistence) ->
    { _, FactInfo } = SueConsistence,
    not lists:member(inconsistent, FactInfo).

solve() ->
    SueInfo = [ convert_string(X) || X <- input()],
    SueConsistence = check_sues(SueInfo,known_facts()),
    SueConsistence2 = check_sues2(SueInfo,known_facts()),
    { FirstSue, _ } = hd(lists:filter(fun consistent_sue/1, SueConsistence)),
    { SecondSue, _ } = hd(lists:filter(fun consistent_sue/1, SueConsistence2)),
    { FirstSue, SecondSue }.
    
