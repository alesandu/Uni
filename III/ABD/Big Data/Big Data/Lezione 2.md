# Verificare l'Identità Polinomiale
Problema: Verificare che $P(x) \equiv Q(x)$

>[!example] Esempio: 
>Verificare che: 
>$(x+1)(x-2)(x+3)(x-4)(x+5)(x-6) \equiv x^6-7x^3+25$

Usiamo $\equiv$ per le identità polinomiali, = per uguaglianze numeriche.
$P(x)\equiv Q(x)$ sse $\forall r \in R, P(r)=Q(r)$.
### Soluzione Deterministica:
$H(x) \equiv (x+1)(x-2)(x+3)(x-4)(x+5)(x-6)$
$G(x) \equiv x^6-7x^3+25$

Trasformare H(x) in forma canonica
$H(x) \equiv \sum_{i=0}^{6}c_ix^i$
$H(x) \equiv G(x)$ sse i coefficenti di tutti i monomi sono uguali.
### Soluzione Randomizzata
1. Scegliere casualmente un intero $r \in [1, ..., 600]$.
2. Calcolare $H(r)$ e $G(r)$.
3. Se $H(r)=G(r)$ allora ritorna "Corretto" altrimenti ritorna "False".  
Scegliamo un numero casual uniformamente dal set $\{a_1,a_2,...,a_k\}$ e assumiamo che quest'operazione richieda 1 step.
L'ouput dipende sulla scelta di $r$, anche se è un variabile random.

Assumiamo in quel $r=2$
$H(2) = 3 * 0 * 5 * -2 * 7 * -4 = 0$ 
$G(2) = 2^6 - 72^3 + 25 = 64 - 56 + 25 = 33$
Dato che $H(2) \neq G(2)$ abbiamo provato che $H(x) \not\equiv G(x)$

Ovviamente quest'algoritmo non è sempre corretto, basti pensare a un valore $r$ che rende l'uguaglianza vera nonostante l'identità polinomiale non è la stessa. 
Quante sono le possibili $r$ sbagliate?

Assumiamo che $G(x) \not\equiv H(x)$, e che la somma di tutti i gradi di $x$ in $H$ e $G$ è vincolata $d$.

>[!abstract] Teorema
>Se
>$F(x) = G(x) - H(x) \not\equiv 0$
>allora l'equiazione
>$F(x) = G(x) - H(x) = 0$
>ha non più di $d$ radici (soluzioni).

# Alcune notazioni di probabilità

>[!example] Definizione
>Uno **spazio di probabilità** ha tre componenti:
>1. Un **spazio di campionamento** $\Omega$, cioè l'insieme di tutti i possibili risultati di un esperimento casuale dallo spaizo di probabilità;
>2. Una **famiglia** $F$ di insiemi, dove ciascun insieme è un sottoinsieme di $\Omega$. 
>3. Una **funzione di probabilità** $Pr: F \rightarrow \mathbb{R}$, che soddisfa le seguenti condizioni:
>	1. $\forall$ evento $E$, $0 \leq Pr(E) \leq 1$;
>	2. $Pr(\Omega)=1$;
>	3. Per ogni sequenza finita o numerabile di eventi mutualmente disgiunti a due a due $E_1,E_2,E_3,...$.
>	   $Pr(\bigcup_{i\geq1}E_i) = \sum_{i\geq1}Pr(E_i)$

La probabilità di un evento è la somma delle probabilità degli eventi semplici.

>[!example] **Definizione**
>Due eventi $E$ e $F$ sono **indipendenti** sse $Pr(E \cap F) = Pr(E) * Pr(F)$ 
>Generalmente, eventi $E_1,E_2,...,E_k$ sono mutualmente indipendete sse ogni sottinsieme $\forall I \subseteq [1,k]$
>$Pr(\bigcap_{i \in I}E_i) = \prod_{i \in I}Pr(E_i)$

>[!example] Definizione
>La **probabilità condizionata** che l'evento $E$ accade sapendo che è avvenuto l'evento $F$ è:
>$Pr(E|F) = \frac{Pr(E \cap F)}{Pr(F)}$ 
>La probabilità condizionata vale se $Pr(F) > 0$

Condizionando $F$ restringiamo lo spazio di campionamento all'insime F.
Due eventi A e B sono indipendenti se 
$Pr(A \cap B) = Pr(A) * Pr(B)$
o
$Pr(A|B) = \frac{Pr(A \cap B)}{B} = Pr(A)$ 

>[!abstract] Teorema (Legge della probabilità totale)
>Siano $E_1,E_2,...,E_n$ eventi mutualmente disgiunti nello spazio di campionamento $\Omega$, e $\bigcup_{i=1}^{n}E_i = \Omega$, allora:
>$Pr(B) = \sum_{i=1}^n Pr(B\ cap E_i) = \sum_{i=1}^n Pr(B|E_i)Pr(E_i)$

>[!quote] Dimostrazione
>L'evento $B \cap E_i$, $i = 1,...,n$ sono disgiunti e coprono l'intero spazio di campionamento $\Omega$.
>
### Legge di Bayes
>[!abstract] Teorema (Legge di Bayes)
>Assumi che $E_1,E_2,...,E_n$ sono inismi mutualmente disgiunti tali che 
>$\bigcup_{i=1}^n E_i = E$, allora:
>$Pr(E_j|B) = \frac{Pr(E_j \cap B)}{Pr(B)} = \frac{Pr(B|E_j)Pr(E_j)}{\sum_{i=1}^n Pr(B|E_i)Pr(E_i)}$

## Analisi dell'algoritmo
Se l'identità è correta, l'algorimto ritorna sempre una risposta corretta. Se l'identità non è corretta, l'algoritmo ritorna la risposta sbagliata solo se scegliamo randomicamente un $r$ il quale è la radice del polinomio $F(x) = G(x) - H(x) = 0$.
Se scegliamo $r \in [1,...,100d]$, la "chance" di ritornare una risposta sbaglia è non più dell' $1\%$. 
Una tecnica randomizzata permette di costruire un algoritmo molto più semplice al costo di una piccola probabilità di errore.

Possiamo ridurre la probabilità di errore al costo di incrementare il run-time dell'algoritmo:
1. Eseguiamo l'algorimto 10 volte
2. Ritorna "Corretto" se abbiamo "Corretto" in tutte e 10 le esecuzioni.
Se il nuovo algoritmo ritorna "Corretto" la "chance" che $G(x) \not\equiv H(x)$ è meno di $10^{-20} < 2^{-64}$. 

Evento semplice = scelta di $r$
Spazio di campionamento = tutti gli interi in $[1,...,100d]$.
Assumiamo che tuti gli interi nel range sono equiprobabili, allora la probabilità dell'evento semplice $r$ è di $Pr(r) = 1/100d$.
Ci sono non più di $d$ eventi "cattivi"
$Pr(evento Cattivo) \leq d/100d$.
Assumiamo di ripetere l'algoritmo $k$ volte:
Se ogni iterazione ritorna "False" allora ritorna "Falso" altrimenti "Corretto".
Un evento semplice = Tutte le sequenze di $r \in [1,...,100d]$  con probabilità = $(1/100d)^k$.
Un evento cattivo = tutte le k scelte sono radici del polinomio, non ci sono più di $d^k$ eventi di questo tipo. Probabilità dell'evento cattivo $\leq d^k(1/100d)^k$.

La probabilità di prendere la radice al primo round è $\leq d/100d$.
Gli eventi in round differenti sono indipendetni.
La probabilità di prendere la radice nei $k$ successivi round è $\leq(d/100d)^k$. 

Per quel che riguarda il tempo e lo spazio necessario per rappresentare $r$, possiamo dire ciò che segue:
- **Spazio**: $r$ è un intero scelto randomicamente in modo unifomre $\in [m]$ (in questo caso con $m = 100d$) dobbiamo rappresentare $r$ come sequenza di bit, per fare ciò ci serviranno $log_2(m)$ bit.
- **Tempo**: per scegliere r randomicamente si usa la seguente tecnica:
  Data la seq di $log_2(m)$ bit per rappresentare $r$ si sceglie randomicamente e in modo uniforme ogni bit (con probabilità $1/2$ o 1 o 0), in totale impieghieremo un tmepo di $O(log_2(m))$.