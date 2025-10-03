/* NOT != 
 * per dire che mario non è amico di giovanni, cio che non scriviamo non è vero,
 * ma per dimostrare che sia falso devo verificare che non sia stato scritto,
 * ?- not amici(mario,rino), */
   
/* devo guardare tutti gli elementi di una base, usiamo:
 * fail -> guardiamo un altro ramo della ricerca,
 * ! -> cut taglia un ramo della ricerca. */

/* p:- 
 * 	a,
 *  !,
 *  b,
 * 	c.
 * p:-
 *  d.
 * il cut taglia l'albero di risoluzione, passato per a vediamo b e poi vediamo c, mettendo il cut,
 * una volta sorpassato il cut non si può più tornare su a, quindi non torneremo su a una volta passato
 * p può essere verificato una sola volta, quindi non posso andare a reistanziare un altra volta
 * (non posso per esempio unificare le nuove istanze in p:- d, sto tagliando lo spazio di ricerca. */

f(b).
f(a).
g(a).
g(b).
g(j).
k(a).

p(A):-
    f(A),
    !,
    g(A),
    %write('1: '),write(A),nl, % write(A) sarebbe il nostro print(A), nl sarebbe new line
    k(A).

/* se mettiamo prima a il ! non farà nulla perché leggera come primo carattere a e solo a è corretta per f,g,k
 * se mettiamo prim b, unifica f con b, g con b e k non ha b quindi darà false 
 * perché al fail non potrà tornare indietro con il cut,
 * se invece leviamo ! ci troverà a (torna indietro, il ramo non è tagliato). 
 * Il comportamento quindi varia in base alla posizione dei nostri rami. */

fallimento_di_g(A):-
    g(A),
    fail.

/* con questo codice guarda tutte i fatti, 
 * invece se levo il fail ogni volta si ferma e dobbiamo noi cliccare next */

/* not deve essere falso se ce ne è almeno una vera ma lasciando il codice 
 * cosi ci darà falso anche per elementi non appartenenti */

fallimento_di_gio(A):-
    g(A),
    !,
    fail.

fallimento_di_gio(_).

/* cosi facendo per ogni variabile ci dà vera ma appena trova una variabile presente fallisce,
 * va nel cut quindi non torna può tornare indietro e va in false */

/* il not in verità viene espresso con \+ */

mynot(Predicato):-
    Predicato,!,fail.
mynot(_).

/* Negation as failure, è vero solo ciò che è detto il resto è falso */

/*
 *nEl([],_,0). 
 * nEl([El|T],El,M):-
 * 	nEl(T,El,N),
 *     M is N+1.
 * nEl([_|T],El,M):-
 *     \+(X=El), % il not poteva essere fatto con =/= o ancor meglio con \+(unifica).
 *     nEl(T,El,M).
 * 
 * sfruttando il cut possiamo evitare di mettere il not */

nEl([],_,0). 
nEl([El|T],El,M):-
    !,
	nEl(T,El,N),
    M is N+1.
nEl([_|T],El,M):-
    nEl(T,El,M).

/* utilizzando il cut perà abbiamo la possibilità di perdere dei dati,
 * con ?-nEl([a,b,a,a,c],V,X) ci viene restituito soltatno le occorrenze del primo elemento che trova
 * */







