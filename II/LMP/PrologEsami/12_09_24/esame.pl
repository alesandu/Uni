/* lun(0,1) =.. [lun,0,1] unificazione lista elementi che lo compongono, serve per generare un nuovo predicato
 * si chiama lumiv, possiamo quindi scirvere:
 * A =.. [lun,0,1] costruendo nella variabile il predicato che ci serve. */

lung([], 0).
lung([_|T], A):-
    lung(T,B),!,
    A is B+1. 

%fatti
haStessaStruttura(P1, P2) :-
    \+ (P1 = (_ :- _)), %verifica non regola senno entra anche nell altro
    \+ (P2 = (_ :- _)), %uguale
    P1 =.. [_|Args1],
    P2 =.. [_|Args2],
    lung(Args1, N1),
    lung(Args2, N2),
    N1 == N2.

%regole
haStessaStruttura((P1 :- Corpo1), (P2 :- Corpo2)) :-
    haStessaStruttura(P1,P2),
    haStessoCorpo(Corpo1, Corpo2).

%verifica se hanno lo stesso numero di fatti
haStessoCorpo(C1, C2) :- 
    C1 =.. [_|Args1],
    C2 =.. [_|Args2],
    lung(Args1, L1),
    lung(Args2, L2),
    L1 == L2,
    stessaStrutturaLista(Args1, Args2).

%controlla accoppiamenti
stessaStrutturaLista([], []). 
stessaStrutturaLista([H1|T1], [H2|T2]) :-
    haStessaStruttura(H1, H2),
    stessaStrutturaLista(T1, T2).
