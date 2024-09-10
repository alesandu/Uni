n(1).
n(7).
n(11).
n(3).
n(11).

/* utilizzando cut,fail, assert e retract
 * creare il predicato numeri(L) (gia esistenti bagof e setof) che ritorna
 * true se unifica la lista in L=[1,3,7,11] */

:- dynamic numeri/1.
:- dynamic appoggio/1.

appoggio([]). %lista di appoggio basde

numeri(L):-
    n(A), %prende un numero
    appoggio(L), %trova un lista L di appostto
    append(L,[A],LN), %concatena il numero alla lista trovata
    retract(appoggio(L)), %elimina la precedente lista
    asserta(appoggio(LN)), %aggiunge la nuova lista
    fail. %scorre tutti i numeri senza canellarli
    
numeri(L):-
 appoggio(L), %alla fine mi trova la lista di appoggio, riga opzionale perche il retract sotto gia unifica L
 retract(appoggio(L)),
 asserta(appoggio([])). 

 /* usiamo ora i predicate begof e setof 
  * 
  * bagof(N,n(N),L). prende: L[1,7,11,3,11]
  * N formato di come vogliamo inserire nella lista 
  * (se abbiamo n(1,a), possiamo passargli pippo(A,L)
  * gli elementi nella lista saranno tutti pippo(1,a) ...),
  * n(N) predicato di cui vogliamo fare l'analisi,
  * L lista riempita. 
  * 
  * setof(N,n(N),L) L=[1,3,7,11] e li ordina anche 
  * 
  * bagof(N,n(N,L),List). tutti i numeri raggruppati per lettera uguale, possiamo anche fare:
  * L=a, bagof(N,n(N,L),List). mi ritorna Lista=[1].
  * 
  * bagof(N,L^n(N,L),List). a patto che esista una L raggruppa i valori, 
  * qualsiasi essa sia, non possiamo unificare L.
  * 
  * */

/* predicato conta quante lettere sono diverse presenti */

n(1,a).
n(7,b).
n(11,b).
n(3,c).
n(8,d).

nld(X):-
    setof(L,N^n(N,L),List),
    length(List,X).
 