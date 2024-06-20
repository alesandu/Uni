/* predicato path(X,Y,Path) vero se esiste un percorso da X a Y e nel path cè il cammino 
 * e(A,B). */

e(a,b).
e(a,d).
e(b,c).
e(d,e).

dfs(X,Y,[X,Y]):-
    e(X,Y).
dfs(X,Y,[X|P_Z_Y]):-
    e(X,Z),
    dfs(Z,Y,P_Z_Y).

/* es: non percorra noti visitati, non superi una certa lunghezza, */

/* per fare una vista bfs possiamo ragionare per livelli */
/* codice non credo funzionante */

pf([],[]).
pf([X|R],F):-
    setof(Z,e(X,Z),RX),
    pf(R,RF),
    append(RX,RF,F).

pf([],[]).
pf([[X|PX]|R],F):-
    setof([Z|[X|PX]],e(X,Z),RX),
    pf(R,RF),
    append(RX,RF,F).
    
apf(F,FR,Y):- %data una frontiera esiste unaì altra forntiera raggiungibile e in questa frontiera ci deve essere y
    pf(F,FR),
    member(Y,FR).

apf(F,FR,Y):-
    pf(F,FRZ),
    apf(FRZ,FR,Y).
    
bfs(X,Y,FF):-
    apf([X],FF,Y).