-module(day23).
-compile(export_all).

% --- Day 23: Opening the Turing Lock ---

% Little Jane Marie just got her very first computer for Christmas
% from some unknown benefactor. It comes with instructions and an
% example program, but the computer itself seems to be malfunctioning.
% She's curious what the program does, and would like you to help her
% run it.

% The manual explains that the computer supports two registers and six
% instructions (truly, it goes on to remind the reader, a
% state-of-the-art technology). The registers are named a and b, can
% hold any non-negative integer, and begin with a value of 0. The
% instructions are as follows:

%     hlf r sets register r to half its current value, then continues
%     with the next instruction.

execute(hlf, a, PC, A, B) ->
    { PC+1, A div 2, B };
execute(hlf, b, PC, A, B) ->
    { PC+1, A, B div 2 };

%     tpl r sets register r to triple its current value, then
%     continues with the next instruction.

execute(tpl, a, PC, A, B) ->
    { PC+1, A * 3, B };
execute(tpl, b, PC, A, B) ->
    { PC+1, A, B * 3 };

%     inc r increments register r, adding 1 to it, then continues with
%     the next instruction.

execute(inc, a, PC, A, B) ->
    { PC+1, A + 1, B };
execute(inc, b, PC, A, B) ->
    { PC+1, A, B + 1 };

%     jmp offset is a jump; it continues with the instruction offset
%     away relative to itself.

execute(jmp, Offset, PC, A, B) ->
    { PC+Offset, A, B };

%     jie r, offset is like jmp, but only jumps if register r is even
%     ("jump if even").

execute(jie, { a, Offset }, PC, A, B) when A rem 2 == 0->
    { PC+Offset, A, B };
execute(jie, { b, Offset }, PC, A, B) when B rem 2 == 0->
    { PC+Offset, A, B };
execute(jie, _, PC, A, B) ->
    { PC+1, A, B };

%     jio r, offset is like jmp, but only jumps if register r is 1
%     ("jump if one", not odd).

execute(jio, { a, Offset }, PC, A, B) when A == 1->
    { PC+Offset, A, B };
execute(jio, { b, Offset }, PC, A, B) when B == 1->
    { PC+Offset, A, B };
execute(jio, _, PC, A, B) ->
    { PC+1, A, B }.

% All three jump instructions work with an offset relative to that
% instruction. The offset is always written with a prefix + or - to
% indicate the direction of the jump (forward or backward,
% respectively). For example, jmp +1 would simply continue with the
% next instruction, while jmp +0 would continuously jump back to
% itself forever.

% The program exits when it tries to run an instruction beyond the
% ones defined.

% For example, this program sets a to 2, because the jio instruction
% causes it to skip the tpl instruction:

% inc a
% jio a, +2
% tpl a
% inc a

test() ->
    Instructions = 
        [ { inc, a },
          { jio, { a, 2} },
          { tpl, a },
          { inc, a } ],
    { _, 2, _ } = start(Instructions),
    ok.
    
% What is the value in register b when the program in your puzzle
% input is finished executing?

solve() ->
    InputInstructions = input(),
    { _, _, B1 } = run({1,0,0}, InputInstructions),
    { _, _, B2 } = run({1,1,0}, InputInstructions),
    { B1, B2 }.

input() ->
    { ok, Binary } = file:read_file("input-day23"),
    FileString = binary_to_list(Binary),
    Lines = string:tokens(FileString, "\n"),
    [ convert(Instruction) || Instruction <- Lines ].
    
register($a) ->
    a;
register($b) ->
    b.

convert([$h, $l, $f, _, R ]) ->
    { hlf, register(R) };
convert([$t, $p, $l, _, R]) ->
    { tpl, register(R) };
convert([$i, $n, $c, _, R]) ->
    { inc, register(R) };
convert([$j, $m, $p, _ | Offset]) ->
    { jmp, list_to_integer(Offset) };
convert([$j, $i, $e, _, R, $,, _ | Offset ]) ->
    { jie, { register(R), list_to_integer(Offset) } };
convert([$j, $i, $o, _, R, $,, _ | Offset ]) ->
    { jio, { register(R), list_to_integer(Offset) } }.


out_of_range(Position,Range) when Position > Range; Position < 1 ->
    true;
out_of_range(_,_) ->
    false.

halting_state(State, Instructions) ->
    { PC, _A, B } = State,
    case out_of_range(PC, length(Instructions)) of 
        true ->
            State;
        false ->
            run(State, Instructions)
    end.

start(Instructions) ->
    run({1,0,0},Instructions).

run(State, Instructions) ->
    { PC, A, B } = State,
    Instruction = lists:nth(PC,Instructions),
    { Op, Data } = Instruction,
    NewState = execute(Op,Data,PC,A,B),
    halting_state(NewState, Instructions).

