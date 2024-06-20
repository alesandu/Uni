vocale('a').
vocale('e').
vocale('i').
vocale('o').
vocale('u').

lung([], 0).
lung([_|T], A):-
    lung(T,B),
    A is B+1.

nV([],0).
nV([El|T],M):-
    vocale(El),!,
	nV(T,N),
    M is N+1.
nV([_|T],M):-
    nV(T,M).

nC([],0).
nC([El|T],M):-
    \+vocale(El),!,
	nC(T,N),
    M is N+1.
nC([_|T],M):-
    nC(T,M).

calcolo(A,B):-
    A = B,!,
    write('Giornata fortunata').
calcolo(A,B):-
    \+A=B,
    write('Giornata sfortunata').
    
giornata(Segno):-
    nV(Segno,A),
    nC(Segno,B),
	V is A/5,
	C is B/16,
    calcolo(V,C).