-module(day11).
-compile(export_all).

% --- Day 11: Corporate Policy ---

% Santa's previous password expired, and he needs help choosing a new
% one.

% To help him remember his new password after the old one expires,
% Santa has devised a method of coming up with a password based on the
% previous one. Corporate policy dictates that passwords must be
% exactly eight lowercase letters (for security reasons), so he finds
% his new password by incrementing his old password string repeatedly
% until it is valid.

% Incrementing is just like counting with numbers: xx, xy, xz, ya, yb,
% and so on. Increase the rightmost letter one step; if it was z, it
% wraps around to a, and repeat with the next letter to the left until
% one doesn't wrap around.

% Unfortunately for Santa, a new Security-Elf recently started, and he
% has imposed some additional password requirements:

%     Passwords must include one increasing straight of at least three
%     letters, like abc, bcd, cde, and so on, up to xyz. They cannot
%     skip letters; abd doesn't count.

%     Passwords may not contain the letters i, o, or l, as these
%     letters can be mistaken for other characters and are therefore
%     confusing.

%     Passwords must contain at least two different, non-overlapping
%     pairs of letters, like aa, bb, or zz.

% For example:

%     hijklmmn meets the first requirement (because it contains the
%     straight hij) but fails the second requirement requirement
%     (because it contains i and l).

%     abbceffg meets the third requirement (because it repeats bb and
%     ff) but fails the first requirement.

%     abbcegjk fails the third requirement, because it only has one
%     double letter (bb).

%     The next password after abcdefgh is abcdffaa.

%     The next password after ghijklmn is ghjaabcc, because you
%     eventually skip all the passwords that start with ghi..., since
%     i is not allowed.

% Given Santa's current password (your puzzle input), what should his
% next password be?

% Your puzzle input is hepxcrrq.

has_increasing([X,Y,Z|_]) when Y =:= X +1, Z =:= Y + 1 ->
    true;
has_increasing([_,Y,Z|T]) ->
    has_increasing([Y,Z|T]);
has_increasing(_) ->
    false.

no_forbidden_letters(String) ->
    not (lists:member($i,String) or lists:member($o,String) or lists:member($l,String)).

has_pair_of(X,[X,X|T]) ->
    has_pair_of(X,[X|T]);
has_pair_of(_,[Y,Y|_]) ->
    true;
has_pair_of(X,[_,Y|T]) ->
    has_pair_of(X,[Y|T]);
has_pair_of(_,_) ->
    false.

has_two_pairs([X,X|T]) ->
    case has_pair_of(X,T) of
        true ->
            true;
        false ->
            has_two_pairs([X|T])
    end;
has_two_pairs([_,X|T]) ->
    has_two_pairs([X|T]);
has_two_pairs(_) ->
    false.

valid_password(Password) ->
    has_two_pairs(Password) andalso has_increasing(Password) andalso no_forbidden_letters(Password).

next_password(Password) ->
    lists:reverse(reversed_next_password(lists:reverse(Password))).

reversed_next_password([$z|T]) ->
    [ $a | reversed_next_password(T) ];
reversed_next_password([L|T]) ->
    [ L+1 | T ].

next_valid_password(Password) ->
    NextPassword = next_password(Password),
    case valid_password(NextPassword) of 
        true -> NextPassword;
        false -> next_valid_password(NextPassword)
    end.
                  
test() ->
    false = valid_password("hijklmmn"),
    false = valid_password("abbceffg"),
    false = valid_password("abbcegjk"),
    "abcdffaa" = next_valid_password("abcdefgh"),
    "ghjaabcc" = next_valid_password("ghijklmn"),
    ok.

solve() ->
    First = next_valid_password("hepxcrrq"),
    Second = next_valid_password(First),
    { First, Second }.
