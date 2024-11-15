# QuickSort Randomizzato
**Input**: Un insieme S.
**Output**: Un insieme S ordinato.
**Algortimo**:
Scegli un elemento uniformemente a caso y da S
Compara tutti gli elementi di S con y. Sia 
$S_1 = \{x \in S - \{y\} | x \leq y\}$,  $S_2 = \{x \in S - \{y\} | x > y\}$
Ritorna la lista:
Q_S($S_1$),y,Q_S($S_2$)

Sia T il numero di comparazione eseguite durante il QuickSort.
>[!abstract] Teorema
>$E[T] = O(nlog)$ 

Sia $s_1,...,s_n$ il numero di elementi ordinati di $S$.
Per $i = 1,...,n$ e $j > i$, si definisce una variabile random $X_{i,j}$ tale che $X_{i,j}=1$ sse $s_i$ è comparato con $s_j$ nell'esecuzione dell'algoritmo, altrimenti $X_{i,j} = 0$.
Il numero di comparazioni eseguendo l'algoritmo è:
$T = \sum_{i=1}^n\sum_{j>i}X_{i,j}$

Siamo interessati in $E[T]$:
Per la probabilità che $X_{i,j} = 1$ abbiamo che $s_i$ è confrontato con $s_j$ sse o $s_i$ o $s_j$ è scelto come pivot prima che venga scelto uno qualsiasi degli $j-i-1$ elementi tra $s_i$ e $s_j$. Gli elementi sono scelti uniformemente in modo casuale $\rightarrow$ gli elementi nell'insieme $[s_i, s_{i+1}, ..., s_j]$ sono scelti uniformemente a caso.
$Pr(X_{i,j}=1) = \frac{2}{j-i+1}$
$E[X_{i,j}] = \frac{2}{j-i+1}$
$E[T] = nlogn + O(n)$

## QuickSort deterministico
**Input**: Un insieme S.
**Output**: L'insieme S ordinato.
**Algoritmo**:
Sia $y$ il primo elemento di $S$.
Confronta tutti gli elementi di $S$ con $y$. Sia
$S_1 = \{x \in S - \{y\} | x \leq y\}$,  $S_2 = \{x \in S - \{y\} | x > y\}$
Ritorna la lista: 
DQ_S($S_1$),y,DQ_S($S_2$)

# Analisi probabilistica del QuickSort
>[!abstract] Teorema
>La complessità spaziale del DQ_S su un input random uniformemente scelte da tutte le possibili permutazioni di S è $O(nlogn)$.

>[!quote] Dimostrazione
>Sia $X_{i,j}$ come prima.
>Se tutte le permutazione sono equiprobabili, tutte le permutazioni di $S_i,...,S_j$ sono equiprobabili, quindi: 
>$Pr(X_{i,j}) = \frac{2}{j-i+1}$
>$E[T] = O(nlogn)$

### Algoritmo Randomizzato:
L'analisi è vero per ogni input, lo spazio di campionamento è lo spazio delle scelte random fatte dall'algoritmo, esecuzioni diverse sono indipendenti.
### Analisi Probabilistica:
Lo spazio di campionamento è lo spazio di tutti i possibili input.
Se l'algoritmo è deterministico esecuzioni ripetute danno lo stesso output.

# Classificazione degli algoritmi
Un algoritmo **Monte Carlo** è un algoritmo randomizzato che può produrre una soluzione non corretta. 
Un algoritmo **Las Vegas** è un algoritmo randomizzato che produce sempre l'output corretto.
In entrambi i casi il tempo di esecuzione è una variabile random.

# Alcune notazioni di probabilità 3
>[!example] Definizione (valore atteso condizionale)
>$E[Y|Z=z] = \sum_y y Pr(Y=y | Z = z)$

>[!danger] Lemma
>Per ogni variabile random X e Y,
>$E[X] = \sum_y Pr(Y=y)E[X|Y=y]$

## Distribuzione Geometrica
>[!example] Definizione (distribuzione geometrica)
>Una variabile geometrica random $X$ con parametro p è dato dalla seguente distribuzione di probabilità su $n = 1,2,...$
>$Pr(X=n)= (1-p)^{n-1}p$

>[!danger] Lemma
>Per una variabile geometrica random con parametro $p$ e $n > 0$
>$Pr(X = n+k | X > k) = Pr(X = n)$

>[!danger] Lemma
>Sia X una variabile random discreta che prende solo valori interi positivi. Allora
>$E[X] = \sum_{i = 1}^{\infty}Pr(X \geq i)$.

Per una variabile geometrica random $X$ con parametro p:
$Pr(X \geq i) = \sum_{n=i}^{\infty}(1-p)^{n-1} = (1-p)^{i-1}$
$E[X] = 1/p$

## Markov Inequality
>[!abstract] Teorema
>**[Markov Inequality]** Per ogni variabile random positiva
>$Pr(X \geq a) \leq \frac{E[X]}{a}$

## Varianza
>[!example] Definizione (varianza)
>La **varianza** di una variabile random X è
>$Var[X] = E[(X - E[X])^2] = E[X^2] - (E[X])^2$

>[!example] Definizione (deviazione standard)
>La **deviazione standard** di una variabile random $X$ è
>$\sigma(X) = \sqrt{Var[X]}$
>
## Chebyshev's Inequality

>[!abstract] Teorema
>Per ogni variabile random
>$Pr(|X-E[X]| \geq a) \leq \frac{Var[X]}{a^2}$
>$Pr(|X-E[X]| \geq a\sigma[X]) \leq \frac{1}{a^2}$
>$Pr(|X-E[X]| \geq \epsilon[X]) \leq \frac{Var[X]}{\epsilon^2(E[X])^2}$

>[!abstract] Teorema
>Se X e Y sono variabili random indipendenti
>$E[XY] = E[X]⋅E[Y]$
>$Var[X+Y] = Var[X] + Var[Y]$



