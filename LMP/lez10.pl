/* definire una struttura alborea e un predicato leaf vero se: 
 * nella struttura cè una foglia che gli diamo per nome. */

/* creiamo un predicato che crea un albero, R è la root e Childern sono i figli di essa 
 * t(R,Children):- 
 * oppure usare 
 * R(Children):..[] */

/* t(3,4) somma(T,S) è vero se in T mettiamo t(3,4) e S abbiamo 7
 * 
 * somma(t(X,Y),S):-
 *  	S is X+Y.  */

/* leaf se è una foglia
/* node se è un qualsiasi nodo */ 

leaf(t(R,[]), R). % ritorna vero se R sono uguali e se R ha [] figli (solo le foglie non hanno figli)

leaf(t(_,Child),L):-
    member(C,Child),
    leaf(C,L).


node(t(R,_),R).

node(t(_,Child),L):- % ritorna ver se R sono uguali e se R ha _(qualsiasi cosa) figli  
    member(C,Child),
    node(C,L).