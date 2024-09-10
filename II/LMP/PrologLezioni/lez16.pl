/* l'albero di derivazione di una grammatica contex free è
 *  uguale all'agoritmo di risoluzione di prolog */    
/*
'A'(L,R1):-
    'B'(L,R),
    'C'(R,R1).

'A'(L,R3):-
    'C'(L,R1),
    'B'(R1,R2),
    'B'(R2,R3).

'B'(['a'|T],T).

'C'(['b'|T],T).
*/
/* è uguale a scrivere: */
 'A'-->'B','C'.
 'A'-->'C','B','B'.
 'B'-->[a].
 'C'-->[b].
/**/