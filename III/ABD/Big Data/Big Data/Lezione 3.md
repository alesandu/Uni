# Moltiplicazione tra Matrici
Date tre matrici $n * n, A, B,$ e $C$ vogliamo verificare che $AB = C$
### Metodo Standard
Moltiplicazione tra matrici - $\theta(n^3)(\theta(n^2.37))$ operazioni.
### Algoritmo Randomizzato:
1. Prendi un vettore random  <span style="text-decoration:overline">r</span> $= (r_1,r_2,...,r_n) \in \{0,1\}^n$.       $O(n)$
2. Calcola $B$<span style="text-decoration:overline">r</span>             $O(n^2)$
3. Calcola $A(B$<span style="text-decoration:overline">r</span>$)$       $O(n^2)$
4. Calcola $C$<span style="text-decoration:overline">r</span>            $O(n^2)$
5. Se $A(B$<span style="text-decoration:overline">r</span>$) \neq C$<span style="text-decoration:overline">r</span> ritorna $AB \neq C$ altrimenti ritorna $AB=C$.
L'algoritmo impiega tempo $\theta(n^2)$.

>[!abstract] Teorema
>Se $AB \neq C$, e $\bar r$ è scelto a caso uniformemente da $\{0,1\}^n$, allora:
>$Pr(AB$ $\bar r$ $= C$ $\bar r$)$) \leq 1/2$

>[!danger] Lemma
>Scegliere $\bar r$ $= (r_1,r_2,...,r_n) \in \{0,1\}^n$ uniformemente a caso è equivalente a scegliere ogni $r_i$ indipendente e uniformemente da $\{0,1\}$

>[!quote] Dimostrazione (Lemma)
>Se ogni $r_i$ è scelto indipendentemente e uniformemente a caso, ognuno dei possibili $2^n$ vettori $\bar r$ è scelto con probabilità $2^{-n}$, dato il lemma.

Dimostrazione correttezza algoritmo:
Sia $D = AB - C \neq 0$  (quindi le due matrici non sono uguali)
$AB$<span style="text-decoration:overline">r</span> $= C$<span style="text-decoration:overline">r</span> implica che $D$<span style="text-decoration:overline">r</span> $= 0$ (se moltiplichi entrambe le matrici per 0 e rendi vera l'uguaglianza significa che la differenza $D$ per il vettore $r$ deve dare 0, cosi da rendere uguali $A$ e $B$).
Poiché $D \neq 0$ allora ha delle entry diverse da zero; assumiamo $d_{11}$.
Per ottenere che $D \bar r$$= 0$ significa che:

$\sum_{j=1}^nd_{1j}r_j = 0$    

La moltiplicazione tra la prima riga della matrice (ci basta solo la prima riga) * vettore, con $d_{11} \neq 0$ deve dare 0.
Tiriamo fuori $d_{11} * r_1$ porto la sommatoria dall'altra parte e divido per $d_{11}$ 
equivalente a:

$r_1 = - \frac{\sum_{j=2}^nd_{1j}r_j}{d_{11}}$

Perciò $d_{11} \neq 0$.
## Principle of Deferred Decision
Assumiamo $r_2,...,r_n$ fissati.
L'RHS (*termine a destra*) è gia determinato, l'unca variabile è $r_1$.

$r_1 = - \frac{\sum_{j=2}^nd_{1j}r_j}{d_{11}}$

Probabilità che $r_1 = RHS$ è non più di $1/2$.
# Approccio Bayesiano
Avendo un modello precedente, con alcuni valori iniziali come parametri del modello modificando il modello, incorporando le nuove osservazioni, otteniamo un modello successivo in grado di catturare le nuove informazioni.
Aumentando il numero di test e aggiornando il modello di conseguenza aumenterà anche la confidenza sull'esito di un algoritmo.