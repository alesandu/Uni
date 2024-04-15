/* 2+3*5 bisogna vedere l'albero sintattico, S (star symbol) 
 * +(2,*(3,5))si fa una vistia sull'albero per capire l'ordine dell'esecuzione 
 * "* : + -" la relazione di precedenza è data così. La relazione di precedenza è transitiva
 * I numeri rispettano questa relazione di precendeza, perciò possiamo associare 
 * una valore numerico a ciascuno di essi: "100 200 300 400" 
 * Nella visita dell'albero verifico se il nuermo sopra è maggiore al nuemro sotto
 * cosi facendo a quali operatori dare la priorità.
 * Un operatore è quindi definito dal nome, dal valore e da come deve apparire. 
 * Possiamo quindi scrivere degli operatori nuovi, tutti gli operatori sono binari
 * quindi si trovano su un albero binario.
 * 
 * :-op(priorità,(fx,xf,xfx,xfy,yfx),nome) 
 * - la priorità deve essere decrescente
 * - posso scegliere tra gli elementi nelle parentesi
 *   xfy indica un operatore associativo a destra (es. A;B;C è A;(B;C))
 *   yfx indica un operatore associativo a sinistra (es. A+B+C è (A+B)+C)
 *   xfx indica un operatore non associativo 
 * - nome è il nome dell'operatore*/

/* Definire l'operatore di somma che deve comportarsi come il + 
 * prednere 2,3,4 e unificarlo a +(X,Y)*/


:-op(300,xfy,+).
:-op(400,xfy,-).
:-op(100,yfx,*).
:-op(200,yfx,/).

somma(X+Y,R):-
  	R is X+Y.

mul(X*Y,R):-
  	R is X*Y.

sott(X-Y,R):-
  	R is X-Y.

div(X/Y,R):-
  	R is X/Y.

/* Dati alcuni fatti:
 * mario ha macchian di dario. gianni ha panino. elena ha panino di giovanni.
 * bisogna rispondere alla domanda: Chi ha Cosa? 
 * definire l'operatore ha e di. */

:-op(200,xfy,ha).
:-op(100,yfx,di).

mario ha macchina di dario. 
gianni ha panino. 
elena ha panino di giovanni.
giacomo ha borsa di pelle di daino.
gennaro ha borsa di pelle di nonna.

/* provare la query X ha Y. */

/* predicati standard che ci dicono se elementi sono unificati o meno 
 * var(X) mi dice se è non è unificato (true) o si (false).
 * nonvar(X) fa il contrario. 
 * ordina(X,LY) vogliamo che la X sia istanziata. */

ordina(X,LX):-
    nonvar(X).
    