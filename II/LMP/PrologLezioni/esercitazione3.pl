/* torre di hanoi */

hanoi([],_,_).

hanoi(h([]))

ordinata([]).
ordinata([_]).
ordinata([H1,H2|T]):-
    H1 > H2,
    ordinata().