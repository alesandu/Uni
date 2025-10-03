ordinata([]).
ordinata([_]).
ordinata([H1,H2|T]):-
    H1 >= H2,
    ordinata([H2|T]).

mattonelle([],_).

mattonelle(([X|A],B),(A,[X|B])):-
    ordinata([X|A]),!,
    ordinata([X|B]),!.