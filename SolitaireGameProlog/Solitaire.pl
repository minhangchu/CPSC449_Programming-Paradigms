% Minh Hang Chu 30074056
% CPSC 449 - Fall 2019 - University of Calgary
% Prof: Robin Cockett
% Assignment Prolog: PEG Solitaire
% Due date: 2019-12-06

% This assignment is completed using prof's codes and TA's hints to solve PEG Solitaire Problem
% Compile and run program by cmd "peg(crossbow) or peg(full) ... "

% Some basic simple boards
% The board is defined with all marble positions and goal positions
board('crossbow',[31, 32, 34, 35, 41, 42, 43, 44, 45, 53],[3]).
board('longbow',[20, 26, 30, 31, 33, 35, 36,41,43,45,52,53,54,63],[3]).
board('notQuiteDead',[2,3,4,12,14,20,21,22,23,24,25,26,30,32,35,36,40,41,42,43,44,45,46,52,54,62,64], [33]).
board('halfDead', [20,22,23,24,30,34,35,40,41,42,43,44,45,52,54,62,64] , [33]).
board('almostDead',[22,23,24,34,35,42,43,44], [33]).
board('full', [2,3,4,12,13,14,20,21,22,23,24,25,26,30,31,32,34,35,36,40,41,42,43,44,45,46,52,53,54,62,63,64], [33]).

% Display board after each moves
% Using member function to check if the peg is on the board and where to place them
% printRow function to print out the row of the board after checking positions
displayBoard(X,O) :-  
    write(=====================), nl,
    write('      '), printRow(2,5,X,O), write('      '),nl,
    write('      '), printRow(12,15,X,O), write('      '),nl,
    printRow(20,27,X,O),nl,
    printRow(30,37,X,O),nl,
    printRow(40,47,X,O),nl,
    write('      '), printRow(52,55,X,O), write('      '),nl,
    write('      '), printRow(62,65,X,O), write('      '),nl.

printRow(End,End,_,_).
printRow(Start,End,Pos,Goal) :-
    Start < End,
    ((member(Start,Pos), write(' x '));
    (member(Start,Goal), write(' o '));
    (not(member(Start,Pos)),(not(member(Start,Goal)), write(' _ ')))),
    NewS is Start + 1,
    printRow(NewS,End,Pos,Goal).

member(X,[X|_]).
member(X,[_|T]) :- member(X,T).

% check if after moves, the peg is still on board
onboard(N) :- 2 =< N, N =< 4.
onboard(N) :- 12 =< N, N =< 14.
onboard(N) :- 20 =< N, N =< 26.
onboard(N) :- 30 =< N, N =< 36.
onboard(N) :- 40 =< N, N =< 46.
onboard(N) :- 52 =< N, N =< 54.
onboard(N) :- 62 =< N, N =< 64.

% jump function to decide the start position, end position and what it jumps over
%this is horizontal jumping upward (to the right)
jump(Start, Jumped, End) :- 
    Jumped is Start+1,
    End is Start+2,
    onboard(Start),
    onboard(Jumped),
    onboard(End).

%horizontally backward (to the left)
jump(Start, Jumped, End) :- 
    Jumped is Start-1,
    End is Start-2,
    onboard(Start),
    onboard(Jumped),
    onboard(End).

%vertically upward (go up)
jump(Start, Jumped, End) :- 
    Jumped is Start-10,
    End is Start-20,
    onboard(Start),
    onboard(Jumped),
    onboard(End).

%vertically downward (go down)
jump(Start, Jumped, End) :- 
    Jumped is Start+10,
    End is Start+20,
    onboard(Start),
    onboard(Jumped),
    onboard(End).

% apply solitaire move and return the final board
solitaire_move(SB,(Start,End),[End|SB2]) :-
    remove(Start, SB, SB1),
    jump(Start,Jumped,End),
    remove(Jumped,SB1,SB2),
    not(member(End,SB2)).

remove(X,[X|T],T).
remove(X,[H|TS],[H|ZS]) :- remove(X,TS,ZS).

% Generate the list of moves to solve the program by taking initial state and final state
% Call indenpendence and pagoda functions to reduce computation
solitaire_steps(GB,[],GB,_).

solitaire_steps(SB, [Mv|Moves], GB, Hist) :-
    solitaire_move(SB, Mv, SB1),
    independent(Mv,Hist),
    findall((P,W), (member(P,[asym,rasym]),weight(P,SB1,W)), Wgts),    
    checkWgts(GB,Wgts),
    solitaire_steps(SB1, Moves, GB, [Mv|Hist]).

%-----------------------------------------------------------------------------------------------------------------------
% Main function to run the program
% type in the name of the game
peg(Board) :-
    board(Board,SB,GB),
    solitaire_steps(SB,Moves,GB,[]),
    solver(SB,GB,Moves),
    !.

% 2 options to see solutions
% The first part is to see the whole solution
% The second part is to prompt users inputs and show step by step
solver(_,_,[]).
solver(SB,GB,[Mv|Moves]) :-
    %print the whole solution
    displayBoard(SB,GB),
    solitaire_move(SB,Mv,SB1),
    displayBoard(SB1,GB),
    solver(SB1,GB,Moves).
    /*
    %prompt users input
    solitaire_move(SB,Mv,SB1),
    write('Press Y to Continue'),nl, read(Answer),nl,
    (Answer = y -> displayBoard(SB1,GB),solver(SB1,GB,Moves);!).
    */
%---------------------------------------------------------------------------------------------------------------
% INDEPENDENCE function
% check if 2 moves are overlap
% or if they are in the right order
independent(_,[]).
independent(Mv,[H|_]) :- 
    overlap(Mv,H),!.

independent(Mv,[H|T]) :-
    lexorder(Mv,H),
    independent(Mv,T).

overlap((Start1,End1),(Start2,End2)) :-
    jump(Start1,Jumped1,End1),
    jump(Start2,Jumped2,End2),
    (Start1 == Start2;
    Start1 == Jumped2;
    Start1 == End2;
    Jumped1 == Start2;
    Jumped1 == Jumped2;
    Jumped1 == End2;
    End1 == Start2;
    End1 == Jumped2;
    End1 == End2). 

lexorder((M1,_),(M2,_)) :-
    M1 < M2.

% -----------------------------------------------------------------------------------------------------------
% PAGODA function

% Pagoda board are used in this program are asymmetric and rotate-asymmetric
pagoda(asym,13,1).
pagoda(asym,20,-1).
pagoda(asym,21,1).
pagoda(asym,23,1).
pagoda(asym,25,1).
pagoda(asym,26,-1).
pagoda(asym,31,2).
pagoda(asym,33,2).
pagoda(asym,35,2).
pagoda(asym,40,-1).
pagoda(asym,41,1).
pagoda(asym,43,1).
pagoda(asym,45,1).
pagoda(asym,46,-1).
pagoda(asym,53,1).

/*

pagoda(simple,13,1).
pagoda(simple,31,1).
pagoda(simple,33,1).
pagoda(simple,35,1).
pagoda(simple,43,1).

pagoda(complex,2,-1).
pagoda(complex,4,-1).
pagoda(complex,12,1).
pagoda(complex,14,1).
pagoda(complex,31,1).
pagoda(complex,32,1).
pagoda(complex,34,1).
pagoda(complex,36,1).
pagoda(complex,52,1).
pagoda(complex,54,1).
pagoda(complex,62,-1).
pagoda(complex,64,-1).


pagoda(top,3,21).
pagoda(top,13,13).
pagoda(top,20,-8).
pagoda(top,21,8).
pagoda(top,23,8).
pagoda(top,25,8).
pagoda(top,26,-8).
pagoda(top,30,5).
pagoda(top,31,5).
pagoda(top,33,5).
pagoda(top,35,5).
pagoda(top,36,5).
pagoda(top,40,-3).
pagoda(top,41,3).
pagoda(top,43,3).
pagoda(top,45,3).
pagoda(top,46,-3).
pagoda(top,53,2).
pagoda(top,54,3).
pagoda(top,63,1).
*/

pagoda(rasym,2,-1).
pagoda(rasym,3,0).
pagoda(rasym,4,-1).
pagoda(rasym,12,1).
pagoda(rasym,13,2).
pagoda(rasym,14,1).
pagoda(rasym,31,1).
pagoda(rasym,32,1).
pagoda(rasym,33,2).
pagoda(rasym,34,1).
pagoda(rasym,35,1).
pagoda(rasym,52,1).
pagoda(rasym,53,2).
pagoda(rasym,54,1).
pagoda(rasym,62,-1).
pagoda(rasym,63,0).
pagoda(rasym,64,-1).

% Calculate weight of the board
weight(_,[],0).
weight(P,[Pos|Rest],Wgt) :- 
    (pagoda(P,Pos,Pweight);Pweight=0),
    !,
    weight(P,Rest,WgtRest),
    Wgt is WgtRest + Pweight.

/*
goal_wgt(crossbow,asym,0).
goal_wgt(longbow,asym,0).
goal_wgt(halfDead,asym,2).
goal_wgt(notQuiteDead,asym,2).
goal_wgt(almostDead,asym,2).
goal_wgt(full,asym,2).


goal_wgt(crossbow,rasym,0).
goal_wgt(longbow,rasym,0).
goal_wgt(halfDead,rasym,2).
goal_wgt(notQuiteDead,rasym,2).
goal_wgt(almostDead,rasym,2).
goal_wgt(full,rasym,2).
*/

checkWgts(_,[]).
checkWgts(G,[(P,WgtP)|Rest]) :-
    weight(P,G,WgtGoal),
%   goal_wgt(G,P,WgtGoal),
    WgtP >= WgtGoal,
    checkWgts(G,Rest).
