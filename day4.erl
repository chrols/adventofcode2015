-module(day4).
-export([test/0, solve/0]).

% --- Day 4: The Ideal Stocking Stuffer ---

% Santa needs help mining some AdventCoins (very similar to bitcoins)
% to use as gifts for all the economically forward-thinking little
% girls and boys.

% To do this, he needs to find MD5 hashes which, in hexadecimal, start
% with at least five zeroes. The input to the MD5 hash is some secret
% key (your puzzle input, given below) followed by a number in
% decimal. To mine AdventCoins, you must find Santa the lowest
% positive number (no leading zeroes: 1, 2, 3, ...) that produces such
% a hash.

% For example:

%     If your secret key is abcdef, the answer is 609043, because the
%     MD5 hash of abcdef609043 starts with five zeroes
%     (000001dbbfa...), and it is the lowest such number to do so.

%     If your secret key is pqrstuv, the lowest number it combines
%     with to make an MD5 hash starting with five zeroes is 1048970;
%     that is, the MD5 hash of pqrstuv1048970 looks like
%     000006136ef....

% Your puzzle input is ckczppom.

search_md5(Key, N, ValidFun) ->
    Md5 = erlang:md5(Key ++ integer_to_list(N)),
    case ValidFun(binary_to_list(Md5)) of
        true ->
             N;
        false ->
            search_md5(Key, N+1, ValidFun)
    end.
    

valid_solution([0, 0, T|_]) when T < 16 ->
    true;
valid_solution(_) ->
    false.    

valid_solution6([0, 0, 0 | _]) ->
    true;
valid_solution6(_) ->
    false.

test() ->
    609043 = search_md5("abcdef",609042, fun valid_solution/1),
    1048970 = search_md5("pqrstuv", 1048960, fun valid_solution/1),
    ok.

solve() ->
    AdventCoin3 = search_md5("ckczppom",1,fun valid_solution/1),
    AdventCoin6 = search_md5("ckczppom",1, fun valid_solution6/1),
    { AdventCoin3, AdventCoin6 }.
