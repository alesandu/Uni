/* torre di hanoi */

hanoi([],_,_).

hanoi(h([X|A],B,C),h(A,[X|B],C)):-
    ordinata([X|A]),!,
    ordinata([X|B]),!,
    ordinata(C).

ordinata([]).
ordinata([_]).
ordinata([H1,H2|T]):-
    H1 > H2,
    ordinata([H2|T]),!.