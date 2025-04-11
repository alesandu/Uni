# Poseidon
Poseidon è una funzione hash progettata specificamente per essere efficiente nei **sistemi a conoscenza zero** (ZKP), come zk-SNARKs e zk-STARKs. La sua struttura è ottimizzata per operazioni in campi finiti grandi (es. $\mathbb{F}_p$), il che la rende resistente agli attacchi classici ma suscettibile a nuove tecniche di analisi.

### 1. Crittanalisi Lineare vs. Differenziale per Poseidon
#### **Crittanalisi Differenziale**:
  - **Più rilevante** per Poseidon perché:
    - Poseidon usa trasformazioni non-lineari (come cubi $x^3$ o $x^5$) che resistono bene alle approssimazioni lineari.
    - La struttura a "permutazione a sostituzione-permutazione (SPN)" è simile a quella di AES, che è più vulnerabile a differenze propagabili.
  - **Possibili attacchi**:
    - Studio di **tracce differenziali** nei round parziali.
    - Analisi della propagazione delle differenze nel campo $\mathbb{F}_p$ (dove le operazioni sono mod $p$).
  
#### **Crittanalisi Lineare**:
  - **Meno efficace** perché:
    - Le non-linearità forti (es. $x^3$) rendono difficile trovare relazioni lineari significative.
    - Le approssimazioni lineari in campi grandi sono statisticamente deboli.

### 2. Risultati Esistenti e Attacchi Noti
Alcuni studi recenti hanno analizzato la sicurezza di Poseidon:
- **Differential Cryptanalysis**:
  - Uno studio ([Grassi et al., 2021](https://eprint.iacr.org/2021/1163)) mostra che Poseidon ha una **resistenza differenziale molto alta** grazie al numero elevato di round e alla scelta delle costanti.
  - Tuttavia, in configurazioni con **round ridotti**, sono stati trovati attacchi differenziali.
- **Algebraic Attacks** (alternativi alla lineare/differenziale):
  - Poiché Poseidon opera su $\mathbb{F}_p$, attacchi basati su **equazioni algebriche** (Gröbner bases, meet-in-the-middle) potrebbero essere più efficaci.

#### **3. Implementazioni e Soluzioni Online**
- **Implementazioni ufficiali**:
  - Il team di Poseidon fornisce codice in [Rust](https://github.com/filecoin-project/neptune) e [C++](https://github.com/HorizenLabs/poseidon2).
  - Alcune librerie ZKP (come [circom](https://github.com/iden3/circomlib) e [arkworks](https://github.com/arkworks-rs)) includono Poseidon.
- **Tool di analisi**:
  - Strumenti come [SageMath](https://www.sagemath.org/) o [GAP](https://www.gap-system.org/) possono essere usati per verificare tracce differenziali.
  - Alcuni ricercatori hanno pubblicato script per testare la sicurezza (es. su [GitHub](https://github.com/) o [IACR ePrint](https://eprint.iacr.org/)).

#### **4. Cosa Usare per Analizzare Poseidon?**
Se vuoi provare un'attacco:
1. **Inizia con la crittanalisi differenziale**:
   - Cerca differenze con probabilità alta dopo pochi round.
   - Usa SageMath per simulare la propagazione in $\mathbb{F}_p$.
2. **Prova attacchi algebrici** se hai familiarità con Gröbner bases.

### **Conclusione**
Per Poseidon, la **crittanalisi differenziale** è più promettente, ma la sua forte progettazione la rende sicura nel caso generale. Se cerchi codici pronti, controlla le implementazioni ufficiali o i repository di ricerca legati a ZKP.  

### Introduzione alle Funzioni Hash Crittografiche  

Le funzioni hash crittografiche sono algoritmi matematici fondamentali per la sicurezza informatica. La loro principale funzione è trasformare un input di lunghezza arbitraria in un output di dimensione fissa, chiamato digest o hash, in modo deterministico. Questo processo garantisce che ogni input abbia un hash unico, rendendo le funzioni hash strumenti essenziali per proteggere dati e comunicazioni.

Questi algoritmi trovano applicazione in diversi ambiti della sicurezza informatica, come la verifica dell’integrità dei dati, la firma digitale, l’autenticazione delle password e la generazione di chiavi crittografiche. Algoritmi noti come SHA-2 e SHA-3 rappresentano gli standard attuali, mentre versioni precedenti come MD5 e SHA-1 sono considerate insicure a causa delle vulnerabilità scoperte nel tempo.  

L’importanza delle funzioni hash crittografiche è cresciuta con l’evoluzione delle minacce informatiche, rendendo essenziale l’utilizzo di algoritmi robusti per proteggere dati e comunicazioni digitali. La scelta di un algoritmo sicuro è cruciale per prevenire attacchi come collisioni o preimmagini, che potrebbero compromettere la sicurezza dei sistemi.

---

## **1. Definizione e Funzionamento**  

Una funzione hash prende in input una stringa di qualsiasi lunghezza e restituisce un output di lunghezza fissa. Ad esempio, l’algoritmo **SHA-256**, appartenente alla famiglia **SHA-2**, trasforma un input di qualsiasi dimensione in una stringa esadecimale di **256 bit (32 byte)**.  

Esempio pratico di hashing con SHA-256:  

```
Input: "ChatGPT"
Output: "e9ef8729431a0eeff79de197c0e8b5f56e09db0333601b21f3d14a7d24c28fa8"
```

Non importa quanto grande sia l'input, il risultato avrà sempre la stessa lunghezza. Inoltre, una piccola variazione nell'input genera un hash completamente diverso (effetto **avalanche**), garantendo che ogni piccola modifica sia rilevabile. Questo comportamento è essenziale per rilevare alterazioni nei dati e garantire l'integrità delle informazioni.

Le funzioni hash crittografiche sono progettate per essere computazionalmente efficienti, consentendo un calcolo rapido anche per input di grandi dimensioni. Tuttavia, devono anche essere resistenti a tentativi di inversione, ovvero non deve essere possibile risalire all'input originale a partire dall'hash.

---

## **2. Proprietà Fondamentali**  

Le funzioni hash crittografiche devono soddisfare tre proprietà fondamentali per essere sicure:  

1. **Resistenza alle collisioni**  
    - Non deve essere possibile trovare due input diversi che generano lo stesso hash. Questa proprietà è cruciale per evitare che due messaggi distinti possano essere interpretati come identici.  

2. **Resistenza alla preimmagine**  
    - Dato un hash, non deve essere possibile risalire all’input originale. Questa proprietà garantisce che l'hash non possa essere decifrato per ottenere i dati originali, proteggendo così la riservatezza delle informazioni.  

3. **Resistenza alla seconda preimmagine**  
    - Dato un input e il suo hash, non deve essere possibile trovare un altro input con lo stesso hash. Questa proprietà impedisce che un attaccante possa creare un messaggio alternativo con lo stesso hash, compromettendo l'integrità dei dati.  

Oltre a queste proprietà, una funzione hash crittografica deve essere **deterministica**, cioè lo stesso input deve sempre generare lo stesso output. Questo garantisce la coerenza e l'affidabilità dell'algoritmo in ogni utilizzo.

---

## **3. Algoritmi Hash Più Diffusi**  

- **MD5**: Lunghezza **128 bit**, oggi considerato **insicuro** a causa della facilità con cui possono essere generate collisioni. Non è più raccomandato per applicazioni critiche.  
- **SHA-1**: Lunghezza **160 bit**, vulnerabile e deprecato. Anche se un tempo ampiamente utilizzato, è stato sostituito da algoritmi più sicuri come SHA-2.  
- **SHA-2**: Famiglia con lunghezze di **224, 256, 384 e 512 bit**, altamente sicuro e ampiamente adottato in applicazioni moderne.  
- **SHA-3**: Basato su **Keccak**, rappresenta un'evoluzione rispetto a SHA-2, offrendo maggiore resistenza agli attacchi quantistici e una struttura interna completamente diversa.  

Ogni algoritmo ha caratteristiche specifiche che lo rendono adatto a determinati contesti. La scelta dell'algoritmo dipende dalle esigenze di sicurezza e dalle risorse computazionali disponibili.

---

## **4. Applicazioni delle Funzioni Hash**  

### **4.1 Verifica dell’Integrità dei Dati**  
- Le funzioni hash vengono utilizzate per verificare che un file o un messaggio non sia stato alterato durante la trasmissione o l'archiviazione. Ad esempio, i checksum basati su hash sono comunemente utilizzati per garantire l'integrità dei file scaricati.  

### **4.2 Firma Digitale**  
- Le firme digitali utilizzano funzioni hash per generare un’impronta digitale del messaggio, autenticandone origine e integrità. Questo processo è fondamentale per garantire la non ripudiabilità e la sicurezza delle comunicazioni.  

### **4.3 Autenticazione delle Password**  
- Le password vengono memorizzate come hash anziché in chiaro, spesso con tecniche di **salting** (aggiunta di valori casuali) e **hashing iterativo** (applicazione ripetuta dell'algoritmo) per aumentare la sicurezza contro attacchi come le rainbow tables.  

### **4.4 Blockchain e Criptovalute**  
- Le funzioni hash sono alla base della sicurezza delle blockchain, proteggendo le transazioni e garantendo l'integrità dei blocchi. Ad esempio, Bitcoin utilizza SHA-256 per il mining e la creazione di nuovi blocchi.  

### **4.5 Generazione di Chiavi Crittografiche**  
- Le funzioni hash vengono impiegate per derivare chiavi crittografiche da una passphrase, garantendo che le chiavi siano uniche e sicure.  

---

## **5. Attacchi e Mitigazioni**  

- **Collisioni**: Due input generano lo stesso hash, compromettendo l'unicità dell'algoritmo.  
- **Preimmagine**: Risalire all’input originale dall’hash, violando la riservatezza.  
- **Rainbow Tables**: Tabelle precompilate di hash utilizzate per decifrare password.  
- **Forza Bruta**: Tentativi sistematici di trovare l’input originale.  

### Mitigazioni  
- Utilizzo di algoritmi sicuri come SHA-3.  
- Applicazione di **salting** per rendere unici gli hash delle password.  
- Uso di **hashing iterativo** per aumentare il costo computazionale degli attacchi.  

---

## **6. Conclusione**  

Le funzioni hash crittografiche sono strumenti essenziali per la sicurezza informatica. Tuttavia, la loro sicurezza dipende dalla scelta di algoritmi robusti e dall’implementazione di buone pratiche. Con l’avanzare della potenza computazionale, è necessario adottare funzioni hash resistenti agli attacchi quantistici, come **SHA-3**.  

---

### **Poseidon2: Una Versione Ottimizzata della Funzione Hash Poseidon**  

Poseidon2 è un'evoluzione della funzione hash Poseidon, progettata per migliorare efficienza e sicurezza nelle applicazioni di **zero-knowledge proofs (ZK)**.  

#### Principali Miglioramenti  

1. **Flessibilità**: Supporta modalità a spugna e compressione, adattandosi a diverse esigenze applicative.  
2. **Efficienza Computazionale**: Riduce il numero di moltiplicazioni e vincoli nei circuiti Plonk, rendendolo ideale per applicazioni ad alte prestazioni.  
3. **Sicurezza Rafforzata**: Resistente a nuovi attacchi algebrici, garantendo una maggiore robustezza rispetto alla versione precedente.  

#### Applicazioni  

- **Alberi di Merkle**: Miglioramenti di performance fino a 5 volte, rendendo Poseidon2 ideale per strutture dati crittografiche.  
- **Protocolli Zero-Knowledge**: Ideale per zk-SNARKs e sistemi di prova, migliorando la scalabilità e la sicurezza.  

---

### **Protocolli a Conoscenza Zero**  

I protocolli a conoscenza zero consentono a una parte (prover) di dimostrare la veridicità di un’affermazione a un’altra parte (verifier) senza rivelare informazioni aggiuntive.  

#### Proprietà Fondamentali  

1. **Completezza**: Se l’affermazione è vera, il verificatore sarà convinto.  
2. **Solidità**: Un prover disonesto non può ingannare il verificatore.  
3. **Zero-conoscenza**: Il verificatore non apprende nulla oltre alla veridicità dell’affermazione.  

#### Applicazioni  

- **Autenticazione**: Dimostrazione dell’identità senza rivelare password.  
- **Blockchain**: Privacy e scalabilità delle transazioni.  

#### Tipologie  

- **Interattivi**: Richiedono interazione continua tra prover e verifier.  
- **Non interattivi**: Consentono verifiche successive senza interazione, utilizzando tecniche come il Fiat-Shamir heuristic.  

In sintesi, i protocolli a conoscenza zero rappresentano strumenti potenti per garantire privacy e sicurezza in ambito crittografico, con applicazioni che spaziano dall’autenticazione alla protezione delle transazioni blockchain.  

