/* rivoltare una lista */

rivoltata([], []).
rivoltata([H|T], RL):-
          append(RT,[H],RL), %sarebbe la nostra concatenazione, definita tra due liste per questo [H]
          rivoltata(T,RT).

/* prova con riv([a,b,c],[c,a,b])
 * H = a, T = [b,c], RL = [c,a,b]
 * goal = append(RT, a, [c,a,b]) da false, perche non finisce con a
 * 
 * prova con riv([a,b,c],[b,c,a])
 * H = a, T = [b,c], RL = [b,c,a]
 * goal = append(RT, a, [b,c,a]) da true, ora bisogna vedere se RT = bc
 * goal = riv([b,c],[b,c]) darà false*/

appartiene(X, [X|_]). 
appartiene(X, [_|T]):-
    appartiene(X,T).

concatenazione([], A, A).
concatenazione([H|T], B, [H|L]):-
concatenazione(T, B, L).

/*
substract(_, [], []).
substract(H, [H|R], R). 
substract(H, [A|R1], [A|R2]):-
    substract(H, R1, R2).
*/
/* primo caso se sottraggo qualsiasi elemento dalla lista vuota ho una lista vuota,
 * se lo inseriamo H può non appartenere alla lista
 * altrimenti H deve appartenere alla lista */
/* secondo caso se H è l'elemento in testa da cancellare ottengo la coda*/
/* se l'elemento H da cancellare è nella coda allora sicuramente la testa di entrambe le liste è la stessa
 * e devo sottrarre l'elemento H dalla lista R1 trovando la lista R2*/
    

permutazione([], []).
permutazione([H|T], B):-
    length([H|T],L), % sia L la lunghezza della lista allora
    length(B,L), % la lunghezza della lista B (lunghezza della permutazione) deve essere la stessa della lista messa in output altrimenti false.
    permutazione(T, PT1_2),
    appartiene(H, B),
    substract(H,B,PT1_2).


lung([], 0).
lung([_|T], A):-
    lung(T,B),
    A is B+1. 