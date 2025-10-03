/* prolog tratta 2+3 come se fosse una classe di equivalenza.
 * +(+(2,3),1) +(A,B).
 * 2+3+1 = A+B unifica ad A = 2+3 e B = 1, questo perché cè un ordine di precedenza.
 * ciò perché crea un albero un albero sintattico di una grammatica di tipo 2 (CF).
 * Nella grammatica viene quindi utilizzato l'operatore is,
 * prima di unificare fa l'operazione richiesta A is 3+1.
 * Istanzia prima la parte destra, cioè ne fa tutte le operazioni, poi la unifica alla parte sinistra.
 * A is A+1 non è un assengamento perciò è come dire 3 = 4 è falso
 */

lung([], 0).
lung([_|T], A):-
    lung(T,B), %teoricamente ci andrebbe un cut sennò va all'infinito nel caso facciamo lung(X,3)*/
    A is B+1. 

/* fare N = M-1 è sbagliato perché predica solamente se partiamo con N istanziato altrimenti errore,
 * errore di non istanziazione significa che non abbiamo gli abbiamo assegnato nessun valore prima 
 */

/*
numerodiEl([],_,0).
numerodiEl([A|_],A,+1).
numerodiEl([_|T], El, N):-
    numerodiEl(T,El,N).
*/ 

nEl([],_,0). % caso base
nEl([El|T],El,M):- % primo caso in cui l'elemento è in testa
	nEl(T,El,N),
    M is N+1.
nEl([X|T],El,M):-
    X \= El, % secondo caso in cui l'elemento è nella coda
    nEl(T,El,M).