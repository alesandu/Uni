/* Induzione strutturale:
 * Data una grande struttura, la divido in sottoparti e
 * ad ognuna di esse applico 
 * le stesse proprietà applicate alla parte più grande.
 * Lista: [a,b,c,d,e]
 * Scrittura della lista: [H|T] cioè testa e coda: 
 * la testa è un elemento
 * la coda è a una lista con gli elementi restanti.
 * 
 * [H|T] = [a,b,c,d,e]: unifico la lista con H = a e T = [b,c,d,e]
 * [H|T] = [a] in questo caso H = a e T = []
 * [H|T] = [] in questo caso è come dire 1=2, non si può unificare 1 a 2 perciò
 * non si puo neanche unificare una lista a niente, il risultato sarà false
 * */

/* per vedere se un elemento x appartiene ad una lista avremo due casi:
 * 1: x è la testa
 * 2: x appartiene alla coda
 * */

/* appartiene(X,L) :-
 *  		L = [H|T],
 *  		X = H.
 * 
 * appartiene(X,L) :-
    		L = [H|T],
    		appartiene(X,T).
 * */


appartiene(X, [X|_]). 

appartiene(X, [_|T]):-
    appartiene(X,T).

/* per capire come fa a predicare dobbiamo ragionare sull'algoritmo di risoluzione
 * ?- appartiene(3|[1,3,2])
 * alla prima richeista da falso 3!=1
 * unifico X con 3 e la T con [3,2]
 * a questo punto abbiamo appartiene(3|[3,2]) che ci darà True
 * */

/*
 * Il principio di cui stiamo parlando è il principio di induzione strutturale
 * (lo posso usare sempre quando lavoro con una struttura, 
 * applico la proprietà che dovrei applicare su tutta la struttura sulle strutture più piccole).
 * */

/* per una lista si possono anche unificare più elementi:
 * [H1,H2|T] = [a,b,c] si ha che H1 = a, H2 = b e T = [a]
 * [H1,H2,H3] = [a,b,c] si ha che H1 = a, H2 = b e H3 = a
 * */
 
/*
 * ?-appartiene(X, [1,2,3]) 
 * Restituisce 1 2 e 3.
 * ?-appartiene(3, L)
 * Resriruisce tutte le liste che possono contenere 3 
 * */ 

/* Questo predicato è vero se A,B,C sono liste e C è la lista concatenata. 
 * Dobbiamo lavorare in induzione strutturale.
 * Per la concatenazione abbiamo il caso base:
 * la concatenazione tra una lista vuota e A deve dare A,
 * il caso induttivo in cui:
 * concatenazione(A,B,C):
 *  A = [H|T],
 *  C = [H|L]
 *  concatenazione(T,B,L)
 * */

concatenazione([], A, A).
concatenazione([H|T], B, [H|L]):-
	concatenazione(T, B, L).