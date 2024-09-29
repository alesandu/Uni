/* lun(0,1) =.. [lun,0,1] unificazione lista elementi che lo compongono, serve per generare un nuovo predicato
 * si chiama lumiv, possiamo quindi scirvere:
 * A =.. [lun,0,1] costruendo nella variabile il predicato che ci serve. */

lung([], 0).
lung([_|T], A):-
    lung(T,B),!,
    A is B+1. 

%rende unica la lista senza tuple
listifica([], []).
listifica([(X, Y)|T], [X, Y|T2]) :-
    listifica(T, T2),!.
listifica([H|T], [H|T2]) :-
    \+ H=(_,_),
    listifica(T, T2),!.

%fatti
haStessaStruttura(P1, P2) :-
    \+ (P1 = (_ :- _)), %verifica non regola senno entra anche nell altro
    \+ (P2 = (_ :- _)), %uguale
    P1 =.. [_|Args1],
    P2 =.. [_|Args2],
    lung(Args1, N),
    lung(Args2, N).

%regole
haStessaStruttura((P1 :- Corpo1), (P2 :- Corpo2)) :-
    haStessaStruttura(P1,P2),
    haStessoCorpo(Corpo1, Corpo2).

haStessoCorpo(C1, C2) :-
    C1 =.. [_|Args1],
    C2 =.. [_|Args2],
    listifica(Args1,Arg1),
    listifica(Args2,Arg2),
    lung(Arg1, L),
    lung(Arg2, L),
    stessaStrutturaLista(Args1, Args2).

%controlla accoppiamenti
stessaStrutturaLista([], []). 
stessaStrutturaLista([H1|T1], [H2|T2]) :-
    haStessaStruttura(H1, H2),
    stessaStrutturaLista(T1, T2).

