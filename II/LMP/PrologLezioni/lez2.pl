/* Un programma prolog è fatto da un insieme di predicati composti da fatti e regole */
/* Le variabili in prolog:
 * (Variabile) bisogna sapere il valore
 * (_variabile) non bisogna sapere il valore
 * (_) possiamo modificare il valore
 * (variabile) è una costante
 * (4) è un numero
 * ('X') diventa una costante*/

/* unifico gli archi */
edge(a,b).
edge(b,c).
edge(c,d).
edge(a,e).
edge(e,f).
edge(f,g).
edge(f,c).

path(X,Y):- %caso base
    edge(X,Y).
    
path(X,Y):- %caso induttivo
    edge(X,Z),
    path(Z,Y).

/* unificare <- assegnare
 * predicare <- funzionare
 * predicato <- funziona
 * induzione <- ricorsione */

/* una ricorsione ha come obiettivo un goal, il codice verrà letto dall'alto verso il basso, 
 * se per caso vogliamo far predicare path(a,d) seguo il codice,
 * prima eseguo caso base: 
 * unifica: path(X,Y) X=a e Y=d
 * goal: edge(a,d)
 * risolvo il goal e il risultato sarà fail,
 * il codice prosegue andando sotto legge il caso induttivo quindi: 
 * unifica: path(X,Y) X=a e Y=d,
 * goal sarà: edge(a,Z) e path(Z,d) 
 * risolvo il goal e per il primo edge dato che abbiamo Z si leggerà dall'alto verso il basso il primo edge(a,Z)
 * il primo edge (a,Z) è edge(a,b) quindi si assegnerà a Z=b
 * unifico: path(X,Y) X=a e Y=b 
 * goal: edge(a,b) e path(b,d)
 * risolvo il goal e per il primo edge sarà vero poi si risolve path(b,d),
 * prima eseguo caso base:
 * unifica: path(X,Y) X=b e Y=d
 * goal: edge(b,d)
 * risolvo il goal e il risultato sarà fail,
 * il codice prosegue con il caso induttivo quindi: 
 * unifica: path(X,Y) X=b e Y=d,
 * goal sarà: edge(b,Z) e path(Z,d)
 * va avanti finché non finisce. */

/* Questo è l'ordine giusto da seugire per inmpostare un predicato, il nostro algoritmo di risoluzione dovrà essere impoststo cosi
 * altrimenti potrà portare a diversi errori come il riempimento dello stack */

/* la dimostrazione è una serie di passaggi giustificati da delle regole, 
 * è possibile passare da regola a regola tramite le regole di inferenza,
 * quantifier elimination (elimino i per ogni) all'unificazione
 * il nostro :- sarebbe un implica al contrario, si torna all'indietro con il MP,
 * quindi path(X,Y) <- edge(X,Y) */