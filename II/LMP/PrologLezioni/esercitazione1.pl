/* esercizio 8 regine */

riga([A,_][A,_]).
colonna([A,_],[_,A]).

diagonale([A,B],[C,D]):-
    X is A-C,
    Y is B-D,
    X =\= Y,
    X =\= -Y,

coppia([_]).
coppia([A,B|T]):-
    riga(A,B),
    colonna(A,B),
    diagonale(A,B),
    coppia([A|T]).

soluzione([_]).
soluzione([H|T]):-
    \+coppia([H|T]),
    soluzione([T]).