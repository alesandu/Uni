## **Introduzione**

Negli ultimi anni, l’emergere di protocolli di privacy e di integrità come SNARKs, STARKs e altri sistemi a prova a conoscenza zero (ZKP) ha richiesto la progettazione di primitive crittografiche ottimizzate per circuiti aritmetici. In questo contesto si collocano gli **algoritmi hash algebraici**, come Poseidon, Rescue, MiMC e Anemoi, progettati per operare efficientemente su campi finiti all'interno di circuiti compatibili con i sistemi ZKP.

Il presente lavoro non ha come obiettivo la valutazione delle prestazioni computazionali o l’efficienza implementativa di tali algoritmi, bensì si concentra sulla **robustezza crittografica** dell’algoritmo Poseidon nei confronti di due attacchi fondamentali: **l’analisi differenziale** e **l’analisi lineare**.

---

## **1. L’algoritmo Poseidon**

Poseidon è una funzione crittografica progettata nel 2019 da Grassi, Rechberger, Katz, Derler e Schofnegger, pensata per offrire **sicurezza crittografica** e **compatibilità aritmetica**. La sua struttura segue un design **SPN (Substitution-Permutation Network)** semplificato, costruito su campi finiti prime \$\mathbb{F}\_p\$.

L'algoritmo si basa sulla seguente struttura iterativa, applicata su uno stato vettoriale \$x = (x\_0, x\_1, ..., x\_{t-1}) \in \mathbb{F}\_p^t\$:

### 1.1 Campi Goldilocks

Per eseguire Poseidon su un campo aritmetico efficiente, si sceglie un **Goldilocks prime**, tipicamente:
$p = 2^{64} - 2^{32} + 1 = 18446744069414584321$
Tale numero è primo, vicino a \$2^{64}\$, e consente operazioni modulari rapide su hardware a 64 bit, rendendo efficiente la composizione aritmetica. Tuttavia, nel presente lavoro **non considereremo** queste ottimizzazioni computazionali, poiché la nostra attenzione è rivolta unicamente alla **resistenza dell'algoritmo agli attacchi crittoanalitici**.

### 1.2 Round Function

Ogni round dell’algoritmo è composto da tre trasformazioni principali:

1. **Addizione di una costante** (Add-Round-Key):
   $x_i \leftarrow x_i + C_i \quad \forall i$
2. **S-box non lineare**:
   $x_i \leftarrow x_i^{\alpha} \quad \text{(tipicamente } \alpha = 5 \text{ o } 3)$
3. **Linear Mixing Layer (MDS matrix)**:
   $x \leftarrow M \cdot x$
   dove \$M\$ è una matrice MDS, ovvero massimamente diffondente.

### 1.3 Round pieni e parziali

* I **round pieni** applicano la funzione S-box a **tutti** gli elementi dello stato.
* I **round parziali** applicano la S-box **solo al primo elemento**, lasciando invariati gli altri, per ridurre il costo computazionale. La presenza alternata di round pieni e parziali è attentamente calibrata per garantire un bilanciamento tra **sicurezza e prestazioni**.

---

## **2. Analisi differenziale e lineare**

### 2.1 Analisi differenziale

L’**analisi differenziale** è una tecnica introdotta da Biham e Shamir (1990) che consiste nello studiare la **propagazione delle differenze** tra coppie di messaggi in input e la corrispondente differenza in output.

#### Formalmente:

Dato un input \$x\$ e una differenza \$\Delta x\$, si osserva la differenza \$\Delta y = F(x \oplus \Delta x) \oplus F(x)\$, cercando **differenze caratterizzanti** che abbiano alta probabilità di propagarsi attraverso i round.

L’obiettivo è costruire **differential trails** con **differential probabilities** sufficientemente elevate da permettere un attacco.

### 2.2 Analisi lineare

L’**analisi lineare** (Matsui, 1993) studia le correlazioni tra combinazioni lineari (XOR nei cifrari bit-oriented, somme nei cifrari field-oriented) di input e output.

#### Formalmente:

Si cercano relazioni del tipo:
$\bigoplus_{i} a_i x_i = \bigoplus_{j} b_j y_j$
che risultino vere con **probabilità significativamente diversa da ½**, indicando un **bias lineare** sfruttabile per un attacco.

Nel contesto di Poseidon, l’analisi lineare si complica per via delle operazioni non-lineari e modulari su campi finiti, rendendo la costruzione di trail meno diretta, ma comunque teoricamente possibile.

---

## **3. Motivazione per l'uso di una versione semplificata di Poseidon**

Al fine di rendere più accessibile l’analisi crittoanalitica, si è deciso di costruire e utilizzare una **versione semplificata di Poseidon**, implementata in Python. Questa versione conserva le caratteristiche strutturali fondamentali dell’algoritmo (S-box, MDS, round parziali/pieni), ma con parametri ridotti:

* Stato vettoriale di dimensione ridotta (\$t = 3\$ o \$t = 4\$)
* Numero di round minore rispetto allo standard
* S-box di grado più contenuto (es. \$\alpha = 3\$)
* Campo finito più piccolo (es. \$\mathbb{F}*{97}\$ o \$\mathbb{F}*{257}\$)

Questa scelta consente di **studiare in dettaglio il comportamento dell’algoritmo** sotto attacco differenziale e lineare, rendendo possibile l’osservazione diretta delle traiettorie critiche e delle correlazioni statistiche, pur sacrificando la sicurezza a livello pratico.

---

## **4. Strategia crittoanalitica**

La fase successiva consiste nel formulare attacchi teorici e sperimentali:

* Costruzione di trail differenziali a partire da un input differenziale fissato
* Analisi della propagazione della differenza attraverso i round
* Calcolo della probabilità differenziale complessiva
* Stima del numero di coppie necessarie per distinguere Poseidon da una funzione casuale

In parallelo, si procederà con:

* Ricerca di correlazioni lineari con bias statisticamente rilevanti
* Calcolo delle probabilità di successo degli attacchi lineari
* Simulazioni numeriche per confermare la validità degli approcci

---
