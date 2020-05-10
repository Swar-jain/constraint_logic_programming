%N Queens Problem
:- use_module(library(clpfd)).
queens(N,Qs):-
    length(Qs, N),
    Qs ins 1..N,
    safe(Qs),
    labeling([ffc], Qs).

safe([]).
safe([Q|Qs]):-
    safe_helper(Qs, Q, 1), safe(Qs).

safe_helper([], _, _).
safe_helper([Q|Qs], Q0, D0):-
    Q0 #\= Q,
    abs(Q0 - Q) #\= D0,
    D1 #= D0 + 1,
    safe_helper(Qs, Q0, D1).


%Sudoku Solver
:- use_module(library(clpfd)).
sudoku(Rows) :-
    length(Rows,9),
    setLines(Rows),
    transpose(Rows,Columns),
    setLines(Columns),
    Rows = [A,B,C,D,E,F,G,H,I],
    blocks(A,B,C),
    blocks(D,E,F),
    blocks(G,H,I),
    flatten(Rows, Soln),
    labeling([ffc], Soln).

setLines([]).
setLines([Row|T]):-
    length(Row,9),
    Row ins 1..9,
    all_distinct(Row),
    setLines(T).

blocks([], [], []).
blocks([A1,A2,A3|Block1], [A4,A5,A6|Block2], [A7,A8,A9|Block3]) :-
        all_distinct([A1,A2,A3,A4,A5,A6,A7,A8,A9]),
        blocks(Block1, Block2, Block3).


problem(1,[[_,_,6, 5,9,_, _,_,_],
          [_,_,3, _,_,_, _,7,_],
          [_,_,_, _,_,_, 5,6,_],
          [_,2,_, 1,7,_, _,_,_],
          [4,8,5, _,_,_, _,_,_],
          [_,6,_, _,_,4, 9,_,_],
          [2,_,_, _,_,5, _,_,8],
          [_,3,8, _,_,1, _,_,_],
          [_,_,_, 3,_,_, 7,5,4]]).


% Map Colouring
:- use_module(library(clpfd)).
color_map(Soln):-
        length(Rs, 6),
        Rs = [A,B,C,D,E,F],
        Rs ins 0..3,
        A #\= D, A #\= F, A #\= C, A #\= B,
        B #\= C, B #\= E,
        C #\= D, C #\= E, C #\= F,
        D #\= E, D #\= F,
    setColors(Rs, Soln), labeling([ffc], Rs).

color(0, red).
color(1, green).
color(2, yellow).
color(3, blue).


map([],_,Sol, Sol).
map([C|CL],N,TempSol, Sol):-
    color(C, X),N1 is N+1, map(CL, N1, [[N,X]|TempSol], Sol).

setColors(X, Soln):-
    map(X,1, [], Soln).


% Zebra Puzzle
:- use_module(library(clpfd)).
solveZebra(Zebra, Water):-
    Nations = [English, Spanish, Ukranian, Norwegian, Japanese],
    Colors = [Red, Green, Yellow, Blue, White],
    Pets = [Dog, Serpent, Fox, Horse, Zebra],
    Drinks = [Coffee, Tea, Milk, Juice, Water],
    Cigarettes = [Winston, Kool, Chesterfield, Luckystrike, Kent],

    Nations ins 1..5,
    Colors ins 1..5,
    Pets ins 1..5,
    Drinks ins 1..5,
    Cigarettes ins 1..5,
        
    all_distinct(Nations),
    all_distinct(Colors),
    all_distinct(Pets),
    all_distinct(Drinks),
    all_distinct(Cigarettes),
       
%constraints
    English #= Red,
    Spanish #= Dog,
    Coffee #= Green,
        
    Ukranian #= Tea,
    abs(White - Green) #= 1,
    Winston #= Serpent,
        
    Yellow #= Kool,
    Milk #= 3,
    Norwegian #= 1,
    abs(   Chesterfield - Fox) #= 1,
    abs(Horse - Kool) #= 1,
    Luckystrike #= Juice,
    Japanese #= Kent,
    abs(Norwegian-Blue) #= 1,

    labeling([ffc], Nations),
    labeling([ffc], Colors),
    labeling([ffc], Pets),
    labeling([ffc], Drinks),
    labeling([ffc], Cigarettes).

    
    
    









