/*10:50*/

riga([A,_],[A,_]).
colonna([_,A],[_,A]).

diagonale([A,B],[C,D]):-
    X is A-C,
    Y is B-D,
    X =\= Y,
    X =\= -Y.

coppia([_]).
coppia([A,B|T]):-
    \+riga(A,B),
    \+colonna(A,B),
    diagonale(A,B),
    coppia([A|T]).

soluzione([_]).
soluzione([H|T]):-
    coppia([H|T]),
    soluzione([T]),!.