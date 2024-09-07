/* Programmazione dinamica in prolog */
/* calcolare Fibonacci */

:- dynamic f/2.

f(0,0).
f(1,1).
f(2,1).

f(N,M):-
    write('a'),nl,
    X is N-1,
    Y is N-2,
    f(X,A),!,
    f(Y,B),!,
    M is A+B,
    asserta(f(N,M):-!).

/* se non avessimo messo l'assert sarebbe entrato dentro fibonacci parecchie volte,
 * invece con l'assert l'aggiunge dentro la memoria come se fosse gia precalcolata,
 * se chiamo f(6,M) la prima volta che la chiederemo calcolerà i fibonacci che vanno da 3 a 6,
 * calcolandolo 4 volte, se dopo lo richiediamo abbiamo asserta(f(N,M)). 
 * che ha inserito i fatti in testa, i fatti in questione sono tutti i fibonacci calcolati precedentemente quindi 
 * richiamando f(6,M) lo avremo gia come fatto. */   

 /* richiamdno molte volte però f(6,M) crearerà molti fatti inutili perciò 
  * al posto di quel asserta è meglio scrivere: asserta(f(N,M):-!) */