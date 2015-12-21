-module(day21).
-export([solve/0]).

% --- Day 21: RPG Simulator 20XX ---

% Little Henry Case got a new video game for Christmas. It's an RPG,
% and he's stuck on a boss. He needs to know what equipment to buy at
% the shop. He hands you the controller.

% In this game, the player (you) and the enemy (the boss) take turns
% attacking. The player always goes first. Each attack reduces the
% opponent's hit points by at least 1. The first character at or below
% 0 hit points loses.

% Damage dealt by an attacker each turn is equal to the attacker's
% damage score minus the defender's armor score. An attacker always
% does at least 1 damage. So, if the attacker has a damage score of 8,
% and the defender has an armor score of 3, the defender loses 5 hit
% points. If the defender had an armor score of 300, the defender
% would still lose 1 hit point.

% Your damage score and armor score both start at zero. They can be
% increased by buying items in exchange for gold. You start with no
% items and have as much gold as you need. Your total damage or armor
% is equal to the sum of those stats from all of your items. You have
% 100 hit points.

% Here is what the item shop is selling:

weapons() -> [% Cost  Damage  Armor
{dagger,        8,     4,       0}, 
{shortsword,   10,     5,       0},
{warhammer,    25,     6,       0},
{longsword,    40,     7,       0},
{greataxe,     74,     8,       0}].

armor() -> [% Cost  Damage  Armor
{none,          0,     0,       0},
{leather,      13,     0,       1},
{chainmail,    31,     0,       2},
{splintmail,   53,     0,       3},
{bandedmail,   75,     0,       4},
{platemail,   102,     0,       5}].

rings() -> [% Cost  Damage  Armor
{none1,       0,     0,       0},
{none2,       0,     0,       0},
{damage1,    25,     1,       0},
{damage2,    50,     2,       0},
{damage3,   100,     3,       0},
{defense1,   20,     0,       1},
{defense2,   40,     0,       2},
{defense3,   80,     0,       3}].

% You must buy exactly one weapon; no dual-wielding. Armor is
% optional, but you can't use more than one. You can buy 0-2 rings (at
% most one for each hand). You must use any items you buy. The shop
% only has one of each item, so you can't buy, for example, two rings
% of Damage +3.

% For example, suppose you have 8 hit points, 5 damage, and 5 armor,
% and that the boss has 12 hit points, 7 damage, and 2 armor:

%     The player deals 5-2 = 3 damage; the boss goes down to 9 hit points.
%     The boss deals 7-5 = 2 damage; the player goes down to 6 hit points.
%     The player deals 5-2 = 3 damage; the boss goes down to 6 hit points.
%     The boss deals 7-5 = 2 damage; the player goes down to 4 hit points.
%     The player deals 5-2 = 3 damage; the boss goes down to 3 hit points.
%     The boss deals 7-5 = 2 damage; the player goes down to 2 hit points.
%     The player deals 5-2 = 3 damage; the boss goes down to 0 hit points.

% In this scenario, the player wins! (Barely.)

% You have 100 hit points. The boss's actual stats are in your puzzle
% input. What is the least amount of gold you can spend and still win
% the fight?

% Answer

% --- Part Two ---

% Turns out the shopkeeper is working with the boss, and can persuade
% you to buy whatever items he wants. The other rules still apply, and
% he still only has one of each item.

% What is the most amount of gold you can spend and still lose the
% fight?


outfits() ->
    [ { Weapon, Armor, Ring1, Ring2 } || Weapon <- weapons(),
                                         Armor <- armor(),
                                         Ring1 <- rings(),
                                         Ring2 <- rings() -- [Ring1] ].



given_damage(Damage, Armor) when Damage =< Armor ->
    1;
given_damage(Damage, Armor) ->
    Damage-Armor.

player_turn(HP, _Damage, _Armor, _BossHP, _BossDamage, _BossArmor) when HP =< 0 ->
    player_dead;
player_turn(HP, Damage, Armor, BossHP, BossDamage, BossArmor) ->
    NewBossHP = BossHP - given_damage(Damage, BossArmor),
    boss_turn(HP, Damage, Armor, NewBossHP, BossDamage, BossArmor).

boss_turn(_HP, _Damage, _Armor, BossHP, _BossDamage, _BossArmor) when BossHP =< 0 ->
    player_wins;
boss_turn(HP, Damage, Armor, BossHP, BossDamage, BossArmor) ->
    NewHP = HP - given_damage(BossDamage, Armor),
    player_turn(NewHP, Damage, Armor, BossHP, BossDamage, BossArmor).
                                  

fight(Outfit, Boss) ->
    { Weapon, Armor, Ring1, Ring2 } = Outfit,
    { _, WCost, WDamage, _ } = Weapon,
    { _, ACost, _, AArmor } = Armor,
    { _, R1Cost, R1Damage, R1Armor } = Ring1,
    { _, R2Cost, R2Damage, R2Armor } = Ring2,
    DamageValue = WDamage + R1Damage + R2Damage,
    ArmorValue = AArmor + R1Armor + R2Armor,
    { BossHP, BossDamage, BossArmor } = Boss,
    Outcome = player_turn(100, DamageValue, ArmorValue, BossHP, BossDamage, BossArmor),
    Cost = WCost + ACost + R1Cost + R2Cost,
    { Outcome, Cost }.    


good_outcome({player_dead, _}) ->
    false;
good_outcome({player_wins, _}) ->
    true.

bad_outcome(X) ->
    not good_outcome(X).
    
evaluate_outfits() ->
    Boss = { 100, 8 ,2}, % Given
    [ fight(Outfit, Boss) || Outfit <- outfits() ].

price_cheapest_win() ->
    Outcomes = evaluate_outfits(),
    GoodOutcomes = lists:filter(fun good_outcome/1, Outcomes),
    { _, Cost } = hd(lists:usort(GoodOutcomes)),
    Cost.
    

price_expensivest_loss() ->
    Outcomes = evaluate_outfits(),
    BadOutcomes = lists:filter(fun bad_outcome/1, Outcomes),
    { _, Cost } = lists:last((lists:usort(BadOutcomes))),
    Cost.
    
solve() ->
    { price_cheapest_win(), price_expensivest_loss() }.
