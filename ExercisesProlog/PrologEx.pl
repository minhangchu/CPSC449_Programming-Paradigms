% Minh Hang Chu - 30074056
% CPSC 449 - Fall 2019 - University of Calgary
% Prof: Robin Cockett
% Prolog exercise

% This exercises is completed with help from TAs and Prof's lecture notes

% Question 1
% 1a - append function - append 2 lists together
% This function is reversible. 
myappend([],Y,Y).
myappend([H|T],Y,[H|Z]) :- myappend(T,Y,Z).

% 1b - reverse - reverse a list, the reverse list should have the same length with original lists
% samelength is used to check 2 lists have the same length
% This function is reversible.

% Check if 2 lists have the same length
samelength([],[]).
samelength([_|T1],[_|T2]) :- samelength(T1,T2).

% Reverse the list
myreverse([],[]).
myreverse([H|T],Z) :-
    samelength([H|T],Z),
    myreverse(T,TS), 
    myappend(TS,[H],Z).

% 1c - flatten 
% This function is NOT reversible
% flatten all - flatten all lists and remove all [] in between
flattenall([], []) :- !.
flattenall([H|Rest], FlatList) :-
    !,
    flattenall(H, List),
    flattenall(Rest, Rest2),
    myappend(List, Rest2, FlatList).
flattenall(L, [L]).

% flatten 1 layer []
my_flatten([], []).
my_flatten([A|B],L) :- islist(A), my_flatten(B,B1), !, myappend(A,B1,L).
my_flatten([A|B],[A|B1]) :- my_flatten(B,B1).

islist([]).
islist([_|T]) :- islist(T).

% 1d - member - check if X is an element of the list
% This function is reversible
% check if x is the first item of the list, otherwise check if x is member of the rest of the list
mymember(X,[X|_]).
mymember(X,[_|T]) :- mymember(X,T).

% 1e -remove - remove element X from the list
% This function is reversible
myremove(X,[X|T],T).
myremove(X,[H|TS],[H|ZS]) :- myremove(X,TS,ZS).


% 2 - member2 - check if an element occurs twice in the list
% use onlyone function to check if element appear once in the list
% member2 uses onlyone to check if element appear twice
% This function is NOT reversible
onlyone(X,[X|Rest]) :- not(member(X,Rest)).
onlyone(X,[Y|Rest]) :- X \= Y, onlyone(X,Rest).

member2(X,[X|Rest]) :- onlyone(X,Rest).
member2(X,[Y|Rest]) :- member2(X,Rest) , X \= Y.

% 3 - substring(X,Y) - succeeds when X occurs as contiguous sublist of Y

substring(X,Y) :- myappend(X,_,T), myappend(_,T,Y), X \= [].

% 4 - sublists - find all sublists of a given lists
% there are 2 ways: 
% 1: write sublist function that gives all sublists and use findall to combine them into 1 list

% use findall
% sublists(X,Y) :- findall(X, sublist(X,Y), X).

% sublist([],_).
% sublist([X|XS], [X|XSS]) :- sublist(XS, XSS).
% sublist([X|XS], [_|XSS]) :- sublist([X|XS], XSS).

% 2: sublists 
sublists([], [[]]).
sublists([H|T], L) :-
    sublists(T, L2),
    addlists(H, L2, L3),
    myappend(L2, L3, L), !.

addlists(_, [], []).
addlists(X, [H|T], [[X|H]|S]) :- addlists(X,T,S).

% 5 - Permutation - succeeds when Y is a permutation of X as a list
% This function is reversible
insert(X,Y,[X|Y]).
insert(X,[H|Y],[H|Z]) :- insert(X,Y,Z).

permutation([],[]).
permutation([H|T],Z):-
    samelength([H|T],Z),
    permutation(T,S),
    insert(H,S,Z).


% 6 - Family tree
% All functions in this question is reversible
% relations
daughter('Mehrad','Sean','Cathy').
son('Mehrad','Sean','Charlie').
son('Mehrad','Sean','Adrian').
son('Eric','Steven','Jacob').

son('Kam','Edward','Eric').
son('Kam','Edward','Mehrad').

% X is grandfather of Y
% X is father of T and T is father/mother of Y
grandfather(X,Y) :-
    (son(X,_,P);daughter(X,_,P)),
    (son(P,_,Y);daughter(P,_,Y);son(_,P,Y);daughter(_,P,Y)).

% X is grandmother of Y
% X is mother of T, and T is father/mother of Y
grandmother(X,Y) :-
    (son(_,X,P);daughter(_,X,P)),
    (son(P,_,Y);daughter(P,_,Y);son(_,P,Y);daughter(_,P,Y)).

% X is sister of Y
% X and Y have the same parents, X is a girl and Y can be boy/girl
sister(X,Y) :-
    daughter(Dad, Mom, X),
    (daughter(Dad, Mom, Y); son(Dad,Mom,Y)),
    X \= Y.

% X is brother of Y
% X and Y have the same parents, X is a boy and Y can be boy/girl
brother(X,Y) :-
    son(Dad, Mom, X),
    (daughter(Dad, Mom, Y); son(Dad,Mom,Y)),
    X \= Y.

% X and Y are siblings
% X and Y have the same parents
siblings(X,Y) :-
    (son(Dad,Mom,X); daughter(Dad, Mom, X)),
    (daughter(Dad, Mom, Y); son(Dad,Mom,Y)),
    X \= Y.

% X is father of Y
% Y is a son/ daughter
father(X,Y) :-
    son(X,_,Y) ; daughter(X,_,Y).

% X is mother of Y
% Y is a son/ daughter
mother(X,Y) :-
    son(X,_,Y) ; daughter(X,_,Y).

% X is cousin of Y
% X and Y have the same grandparents
% But they have different parents
cousin(X,Y) :-
    grandfather(T,X),grandfather(T,Y),grandmother(G,X),grandmother(G,Y),
    father(U,X),father(Dad,Y),mother(A,X),mother(Mom,Y),
    U \= Dad, A \= Mom.
    

% 7 - Path and shortest path for directed graph
%relations
edge(a,b).
edge(a,c).
edge(a,d).
edge(e,a).
edge(c,a).

edge(b,c).
edge(c,d).
edge(d,e).

% Decide if there is a path between 2 nodes
path(X,Y) :- findpath(X,Y,[]).

findpath(Start, End ,List) :-
    edge(Start,T),
    not(member(T,List)),
    (T = End; findpath(T,End,[Start|List])).

%7b shortpath 
% Find the shortest path between 2 nodes
% Using depth first search
% Based on Prof's notes in lecture

shortpath(X,Y,Z) :- itsearch(X,Z,Y).

itsearch(Start,(Path,Cost),End):-
    isearch(0,Start,Path,End,Cost).

isearch(Bound,Start,P,End,Bound):-
    dfsearch(Bound,Start,P,End),!.             % finish if df search succeeds within bound
isearch(Bound,Start,P,End,Cost):-
    NextBound is Bound+1,                      % otherwise increment the bound
    isearch(NextBound,Start,P,End,Cost).

dfsearch(_,End,[],End):- !.
dfsearch(Bound,Start,[(Start,Next)|Rest],End):-
    edge(Start,Next), 
    1 =< Bound,                                 %  bound the depth
    NewBound is Bound-1,           
    dfsearch(NewBound,Next,Rest,End).


% 8 - Puzzle solver
% FUnctions in this question is not reversible
% type "solver(houses,X)." to get the answer for questions
% Question: Who owns the hamster?
% Question: Who drinks orange juice?
% General code
solver(Name,Solution):- get_puzzle(Name,P),solve_puzzle(P,Solution).

solve_puzzle(puzzle(Clues,Queries,Solution),Solution):-
     solve(Clues),
     solve(Queries).

solve([Clue|Clues]):- Clue, solve(Clues).
solve([]).

get_puzzle(Name,puzzle(Clues,Queries,Solution)):-
    structure(Name,Structure),
    clues(Name,Structure,Clues),
    queries(Name,Structure,Queries,Solution).

% Puzzle
% There are five houses in a row,
% each of which are a different color and are owned by a men of different nationalities, who
% own different pets, prefer different drinks, and play a different sport.

structure(houses,([house(_,_,_,_,_),house(_,_,_,_,_),house(_,_,_,_,_),house(_,_,_,_,_),house(_,_,_,_,_)])).

% Clues:
% (a) The Irishman lives in the first house on the left.
% (b) The man who plays baseball lives in the house next to the man who keeps a tiger.
% (c) The occupant of the house, next to the house where the owner rides a horse, plays soccer.
% (d) The squash player drinks gin.
% (e) The Frenchman plays rugger.
% (f) The Irishman lives next to the blue house.
% (g) The Englishman lives in the red house.
% (h) The Spaniard is often seen taking his dog for a walk.
% (i) Beer is brewed (and drunk in large quantities) in the green house.
% (j) The Scotsman drinks whiskey and is often tipsey.
% (k) The green house is immediately to the right of the white house.
% (l) The tennis player owns snakes.
% (m) Soccer is played in the yellow house.
% (n) A lot of wine get consumed in the middle house

clues(houses,Houses,
      [
      (nationality(P1C1,irishman), firstleft(P1C1,Houses)),
      (sport(P1C2,baseball),pet(P2C2,tiger),nextto(P1C2,P2C2,Houses)),
      (sport(P1C3,soccer),pet(P2C3,horse),nextto(P1C3,P2C3,Houses)),
      (drink(P1C4,gin),sport(P1C4,squash),anywhere(P1C4,Houses)),
      (nationality(P1C5,frenchman),sport(P1C5,rugger),anywhere(P1C5,Houses)),
      (nationality(P1C6,irishman),color(P2C6,blue),nextto(P1C6,P2C6,Houses)),
      (nationality(P1C7,englishman),color(P1C7,red),anywhere(P1C7,Houses)),
      (nationality(P1C8,spaniard),pet(P1C8,dog),anywhere(P1C8,Houses)),
      (drink(P1C9,beer),color(P1C9,green),anywhere(P1C9,Houses)),
      (nationality(P1C10,scotsman),drink(P1C10,whiskey),anywhere(P1C10,Houses)),
      (color(P1C11,green),color(P2C11,white),right(P1C11,P2C11,Houses)),
      (pet(P1C12,snake),sport(P1C12,tennis),anywhere(P1C12,Houses)),
      (color(P1C13,yellow),sport(P1C13,soccer),anywhere(P1C13,Houses)),
      (drink(P1C14,wine),middle(P1C14,Houses))
      ]
      ).

% Predicates
nationality(house(A,_,_,_,_),A).
color(house(_,B,_,_,_),B).
sport(house(_,_,C,_,_),C).
pet(house(_,_,_,D,_),D).
drink(house(_,_,_,_,E),E).     

right(A,B,[B,A,_,_,_]).
right(A,B,[_,B,A,_,_]).
right(A,B,[_,_,B,A,_]).
right(A,B,[_,_,_,B,A]).

firstleft(A,[A,_,_,_,_]).
middle(A,[_,_,A,_,_]).
      
nextto(A,B,[A,B,_,_,_]).
nextto(A,B,[_,A,B,_,_]).
nextto(A,B,[_,_,A,B,_]).
nextto(A,B,[_,_,_,A,B]).
nextto(A,B,[B,A,_,_,_]).
nextto(A,B,[_,B,A,_,_]).
nextto(A,B,[_,_,B,A,_]).
nextto(A,B,[_,_,_,B,A]).

anywhere(A,[A,_,_,_,_]).
anywhere(A,[_,A,_,_,_]).
anywhere(A,[_,_,A,_,_]).
anywhere(A,[_,_,_,A,_]).
anywhere(A,[_,_,_,_,A]).


% Queries - answer questions
queries(houses, Houses,
        [ member(Q1,Houses),
          nationality(Q1,Nation),
          pet(Q1,hamster),

          member(Q2,Houses),
          nationality(Q2,Nation2),
          drink(Q2,orangejuice)
        ],
   % The solution
         [
         ['This guy owns the hamster ',Nation],
         ['The guy drinks orange juice ', Nation2]
         ]).

% 9 - Josephus circle
% Functions is this question are not reversible
% type "josephus(number_of_people, count_num, X)." Function will return a list of positions of last 2 people survived

% They stood in a circle and counted down clockwise
% from a given number N (and a starting position): the man who uttered 0 then killed himself
% and the circle as a result shrank and the whole procedure was repeated starting at the man
% who stood (clockwise) next to the spot just vacated.

% makelist is used to make a list of people in the circle
% loop is used to determine who survive after counts
% call josephus with number of people and count number to find position where they should place themselves

josephus(People,Count,SurviveList) :- makelist(People,0,List) , loop(List,Count,Count,SurviveList).

makelist(People,Num,[]) :- People = Num.
makelist(People,Num,List) :-
    (People > Num, Num2 is Num + 1, myappend([Num2],List2,List), makelist(People,Num2,List2)).

loop([H|T],0,Count,SurvivalList) :-
    (samelength([H|T],[0,1]) ->
    SurvivalList = [H|T]);
    loop(T,Count,Count,SurvivalList).

loop([H|T],C,Count,SurvivalList):-
    C > 0, NewC is C-1, myappend(T,[H],Newlist),loop(Newlist,NewC,Count,SurvivalList).