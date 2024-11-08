# Introduzione al Data Mining

**Obiettivo:** Trovare il miglior hardware, i migliori modelli, i migliori algoritmi e il miglior software per risolvere problemi su Data Set di grandi dimensioni e/o complessi.

## Fasi del Data Mining

1. **Fase I:**  
   Trasformare il dataset in un modello/struttura formale I  per ridurre il problema originale in una task computazionale ben definita T. Questa fase è spesso la più impegnativa.
2. **Fase II:**  
   Trovare il miglior algoritmo per risolvere T su input I. Una volta risolta la fase 1, la fase 2 richiede soltanto metodi standard.

## Problema Reale di Data-Mining: Phishing Emails

### Soluzione Algoritmica
1. Classificare le email ricevute in due sottoinsiemi: **Phishing** e **Non-Phishing**.
2. Estrarre le parole (o frasi) che appaiono insolitamente spesso nelle mail di phishing (es. "send money").
3. Assegnare pesi positivi alle parole di phishing e pesi negativi alle altre.
4. **Algoritmo:** Per ogni email in ingresso, calcolare la somma dei pesi delle parole. Se la somma è maggiore rispetto a una certa soglia, allora la mail viene classificata come **Phishing**.

### Task Complessi
2. Definire un modello statistico M (es. distribuzione di probabilità) sull'input di dati affinché l'informazione nascosta possa emergere come evento probabile in M, successivamente estrarre queste informazioni in maniera efficiente.
3. Assegnare i giusti pesi alle parole affinché la somma totale delle mail in ingresso sia proporzionale alla probabilità che sia phishing.

## Statistical Modeling (Definizione Informale)

Costruzione di una distribuzione di probabilità sottostante al modello, dalla quale vengono campionati i dati visibili "grezzi".

>[!example] **Esempio:**  
> Se il dataset grezzo è un insieme D di numeri, un modello statistico per D è dato dall'assunzione che i numeri vengano campionati secondo una distribuzione Gaussiana. In questo caso, la media e la deviazione standard caratterizzano completamente la distribuzione.

## Adversarial Data Models (Definizione Informale)

In altri scenari tipici nel data mining, si adotta l'ipotesi del caso peggiore: 

L'Input Data Set, da cui deve essere estratta l'informazione cercata, è gestito da una fonte avversaria che genera i dati con l'obiettivo di minimizzare l'efficienza della soluzione algoritmica utilizzata o la sua validità.

>[!example] **Esempio:**  
> Calcolare (e aggiornare) il numero di 1 nell'ultima finestra di lunghezza N su un flusso infinito di bits gestito da un avversario, il quale sceglie il prossimo bit in funzione delle precedenti scelte fatte dall'algoritmo.

## Data Mining e Machine Learning

**Machine Learning (ML)** risulta spesso un ottimo approccio al data mining. Consiste nell'usare parte del Data Set in Input come **Training Set** per istruire i sistemi ML. 

ML è utile in situazioni dove non è possibile definire una funzione obiettivo chiara sul Data Set, ossia quando non si sa cosa l'Input Data dice in merito al problema da risolvere.

## Limiti Statistici sul Data Mining

L'obiettivo tipico dei problemi di data mining è individuare eventi inusuali nascosti all'interno di dataset di grandi dimensioni. 

Quando un dataset contiene un gran numero di elementi ottenuti da diverse fonti, si possono verificare eventi inusuali, i quali possono non avere un particolare significato, dato che sono semplici artefatti statistici.

## Principio di Bonferroni

Se si cerca uno specifico evento E all'interno di un DataSet DS in Input, ci si può aspettare che tale evento accada anche se DS è completamente casuale, e che il numero di occorrenze di tale evento aumenti al crescere della dimenisone di DS. Queste occorrenze, chiamate **falsi** (o "bogus"), il loro verificarsi non ha nessuna causa significativa sennon dati casuali.
Il Toerema Bonferroni Correction permette di evitare la maggior parte dei falsi positivi.

## Esempio di Bogus: Gangs in Social Network

**Graph Model:**  
Sia G = (V, E) un grafo, dove V è un insieme di n agenti (potenzialmente criminali) che visitano un insieme H di h luoghi pubblici in una città. 
L'arco (u, v) appartiene a E se u  e v visitano lo stesso luogo lo stesso giorno per una sequenza di T >> 0 giorni.
**Hypothesis Model**:
Si assume che il processo di visita degli agenti nei vari luoghi sia casuale e uniforme.
**Problema**:
Individuare il numero di possibili "Gangs", chiamate **Cliques** in G.

## Gangs in Social Network: Impostazione Formale

**Domanda:** Qual è la dimensione massimale di cliques in G(V, E)?
**Risposta:** Per $\forall(u, v) \in V$, abbiamo che: 
$Pr[(u, v) \in E] \approx T/h \overset{\Delta}{=} p$.
Per un sottoinsieme $S \subseteq V$ di dimensione $s$, otteniamo:
$Pr[S \text{ è una clique}] \approx p^{{s^2}/{2}}$
quindi:
$E[\# \text{ cliques di dimensione } s] = C(n, s) \cdot p^{{s^2}/{2}} \approx ({{n/e \cdot s}})^s \cdot (p^{{s^2}/{2}})$
Osservando un sistema spazio-tempo grande, ad esempio $n \approx 10^7$, $h \approx 10^4$, $p \approx 1/10$, ci possiamo aspettare un grande numero di cliques di dimensione $s \approx 10$. 
Quindi, una gang di 10 agenti che si sono incontrati almeno una volta non è un evento sorprendente.

**Approccio al Data Mining**  
L'approccio è algoritmico: dato un Data Set DS, fornire un algoritmo (e Data Structure) efficiente per rispondere a query complesse su DS.

>[!example] **Esempio**  
> Dato un dataset DS di interi, mantenere il valore medio del DS (visto finora) e la sua deviazione standard.

L'algoritmo non dovrebbe basarsi su alcuna ipotesi statistica I su DS, ma se I vale, i valori calcolati saranno coerenti con I.

## Typical Tasks

1. Computational Modeling dei Data Processes (Input).
2. Sintetizzare un Data Set.
3. Estrarre le caratteristiche più importanti del dataset e scartare il resto.

# Data Streams

In molte situazioni di Data Mining, inizialmente non conosciamo l'intero Data Set. La Stream Management è importante quando il rate di input è controllato dall'esterno, come in:
- Query su motori di ricerca
- Aggiornamenti di stato su Twitter o Facebook
- Indirizzi IP gestiti dai server
Possiamo pensare al Data Set come infinito e non-stazionario (la distribuzione cambia nel tempo).

## Modello Data-Stream

Gli elementi in Input entrano a un tasso elevato, su una o più porte di Input (tuple). Il sistema non può mantenere l'intero Stream S in memoria, ma solo piccoli sketches di S possono essere conservati e aggiornati.

## Problemi su Data Stream

Tipi di query su un Data Stream DS:
- Campionare dati da un DS
- Filtrare un DS
- Contare elementi distinti in un DS
- Stimare momenti (valore medio e deviazione standard)
- Trovare i k elementi più frequenti visti finora

## Summarization

Dato un grande dataset DS, fornire un piccolo sketch H(S) che riassuma efficacemente le caratteristiche principali del DS.
**Sfide di Ottimizzazione:**
- Minimizzare la dimensione |H(S)|
- H(S) deve approssimare efficacemente le caratteristiche chiave del DS
- H(S) deve permettere un aggiornamento dinamico efficiente

## Feature Extraction

L'estrazione di caratteristiche da enormi dataset si basa sul selezionare le dipendenze statistiche (correlazioni) più importanti tra gli elementi e usare solo queste per rappresentare il dataset.
