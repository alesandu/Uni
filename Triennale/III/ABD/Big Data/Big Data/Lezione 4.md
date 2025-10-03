# Min-Cut Algorithm
**Input**: Un grafo $G=(V,E)$ con $|V| = n$.
**Output**: Un insieme minimale di archi che disconnette il grafo.
```
	Ripeti n-2 volte:
		Prendi un arco uniformemente a caso
		Contrai i due vertici connessi dall'arco ed elimina tutti gli archi che collegano i due vertici.
	Ritorna l'insieme di archi che connettono i due vertici rimanenti
```

>[!abstract] Teorema
>L'algoritmo ritorna l'insieme min-cut di archi con probabilità $\geq \frac{1}{n(n-1)}$

>[!danger] Lemma
>La contrazione dei vertici non riduce la grandezza del min-cut. (La contrazione può solo aumentarne la grandezza). 

>[!quote] Dimostrazione
>Ogni cut nel nuovo grafo è un cut del grafo originale.

## Analisi dell'algoritmo
Assumiamo che il grafo ha un min-cut di $k$ archi.
Possiamo calcolare la probabilità di trovare un insieme simile Scegliere $C$.

>[!danger] Lemma
>Se gli archi contratti non appartengono a C, nessun arco eliminato in quel passo appartiene a C.

>[!quote] Dimostrazione
>Una contrazione elimina un insieme di archi paralleli (archi che collegano una coppia di vertici). Tutti gli archi paralleli o appartnegono, o non appartengono a $C$.

Sia $E_i =$ l'arco contratto nell'iterazione $i$ non appartiene a $C$.
Sia $F_i = \bigcap^{i}_{j=1}E_j=$ nessun arco di C è stato contratto nelle prime $i$ iterazioni.
Dobbiamo calcolare $Pr(F_{n-2})$
Dato che il minimo cut-set ha $k$ archi, tutti i vertici hanno grado $\geq k$, e il grafo ha $\geq nk/2$ archi.
Ci sono almeno $nk/2$ archi nel grafo, $k$ archi sono in $C$.
$Pr(E_1) = Pr(F_1) \geq 1 - \frac{2k}{nk} = 1 - \frac{2}{n}$.

Assumiamo che la prima contrazione non ha eliminato nessun arco di $C$ (condizione dell'evento $E_1=F_1$).
Dopo la prima contrazione rimane un grafo con $n-1$ nodi, con il minimo cut set, e grado minimo $\geq k$.
Il nuovo grafo ha almeno k(n-1)/2 archi.
$Pr(E_2|F_1) \geq 1 - \frac{k}{k(n-1)/2} \geq 1 - \frac{2}{n-1}$
Similmente:
$Pr(E_i|F_{i-1}) \geq 1 - \frac{k}{k(n-i+1)/2} \geq 1 - \frac{2}{n-i+1}$
Dobbiamo calcolare $Pr(F_{n-2})$ e usiamo $Pr(A \bigcap B) = Pr(A|B)Pr(B)$
$Pr(F_{n-2}) = \frac{2}{n(n-1)}$.

>[!abstract] Teorema
>Assumiamo di runnare l'algorimto randomizzato del min-cut $n(n-1)logn$ volte e ritorna la grandezza minima del cut-set trovato in ogni iterazione. La probaiblità che l'output non è un min-cut è legato da:
>$(1-\frac{2}{n(n-1)})^{n(n-1)logn} \leq e^{-2logn} = \frac{1}{n^2}$

>[!quote] Dimostrazione
>L'algoritmo ha un errore: l'output non è mai più piccolo del valore del min-cut.

# Alcune nozioni di probabilità 2
>[!example] Definizione (variabile aleatoria)
>Una variabile aleatoria X su uno spazio di campionamento Ω è una funzione su Ω a valori reali (e quindi misurabile), ossia X : Ω $\rightarrow \mathbb{R}$. Una variabile aleatoria discreta è una variabile aleatoria che assume solo un numero finito o un numero infinito numerabile di valori.

>[!example] Definizione (indipendenza)
>Due v.a. $X$ ed $Y$ sono **indipendenti** se e solo se $Pr((X = x) \cap (Y = y)) = Pr(X = x) ⋅ Pr(Y = y)$ Analogamente, le variabili aleatorie $X_1 , X_2 , . . . , X_k$ sono mutualmente indipendenti se e solo se, $\forall I \subseteq [1, k]$ e $\forall x_i$ con $i \in I$ 	
>
>$Pr(\bigcap_{i \in I }X_i = x_i) = \prod_{i \in I}Pr(X_i = x_i)$

>[!example] Definizione (valore medio)
>Il **valore atteso** di una variabile aleatoria discreta $X$, denotato $E[X]$, è dato da $E[X] = \sum_i iPr(X = i)$ dove la sommatoria è definita su tutti i valori nel range di $X$. Il valore medio è finito se la sommatoria converge, altrimenti è illimitato.

Il valore medio è una sommatoria pesato su tutti i possibili valori dell v.a.
>[!example] Definizione (mediano)
>Il **mediano** di una v.a. $X$ è un valore $m$ t.c. 
>$Pr(X < m) \leq 1/2$ e $Pr(X > m) < 1/2$

Una proprietà fondamentale del valore atteso, che può semplificare in maniera significativa il suo calcolo, è la linearità del valore atteso. Da questa proprietà, si deriva che il valore atteso della somma di v.a. è uguale alla somma dei loro valori attesi
>[!abstract] Teorema
>Siano $X$ e $Y$ due v.a. allora
>$E[X+Y] = E[X] + E[Y]$

>[!danger] Lemma
>Per ogni costante $c$ e v.a. $X$
>$E[cX] = cE[X]$
>
### Variabile Aleatoria Bernulli
Un Bernulli o una v.a. indicatrice:
$Y = \begin{cases} 1 & \mbox{se l'esperimento ha successo}\\0 & \mbox{altrimenti} \end{cases}$
$E[Y] = p ⋅ 1 + (1-p)  ⋅  0 = p = Pr(Y = 1)$
### Variabile Aleatoria Binomiale
>[!example] Definizione
>Una **variabile aleatoria binomiale** $X$ con parametri $n$ e $p$, scritta $B(n,p)$ è definita dalla seguente distribuzione di probabilità su $j = 0,1,2,...,n$:
>$Pr(X = j) = \binom{n}{j}p^j(1-p)^{n-j}$

E[X] = np