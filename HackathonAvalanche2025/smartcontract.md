# Documentazione Smart Contract: SkillWager

Questo documento descrive in dettaglio il funzionamento dello smart contract `SkillWager.sol`, il cuore della piattaforma di scommesse e-sports decentralizzata.

## Panoramica
Il contratto gestisce l'intero ciclo di vita di una scommessa (match), dalla creazione alla risoluzione, inclusa la gestione dei fondi (wager + bond) e la risoluzione delle dispute tramite una giuria o un amministratore.

## Strutture Dati Principali

### Stati del Match (`MatchState`)
Ogni match può trovarsi in uno dei seguenti stati:
- **OPEN**: Il match è stato creato da un giocatore (Player A) ed è in attesa di un avversario.
- **LOCKED**: Un secondo giocatore (Player B) si è unito. I fondi sono bloccati nel contratto.
- **DISPUTE_L1**: C'è disaccordo sui risultati o è stato richiesto un intervento per timeout. La giuria è chiamata a votare.
- **DISPUTE_L2**: (Riservato per escalation future, attualmente non pienamente implementato nel flusso base).
- **RESOLVED**: Il match è concluso e i fondi sono stati distribuiti.
- **CANCELLED**: Il match è stato annullato e i fondi rimborsati.

### Risultati (`Result`)
I giocatori possono dichiarare:
- **NONE**: Nessun risultato dichiarato.
- **WIN**: Vittoria.
- **LOSS**: Sconfitta.

### Struttura `Match`
Ogni match contiene:
- `id`: Identificativo univoco.
- `playerA` / `playerB`: Indirizzi dei due giocatori.
- `wagerAmount`: Importo della scommessa.
- `bondAmount`: Importo della cauzione (anti-cheating).
- `state`: Stato corrente.
- `resultA` / `resultB`: Risultati dichiarati dai giocatori.
- `jurors`: Lista dei giurati assegnati (per la demo sono mock jurors).
- `votesForA` / `votesForB`: Conteggio voti della giuria.

## Flusso Operativo

### 1. Creazione del Match (`createMatch`)
- **Chi**: Player A.
- **Azione**: Invia una transazione con valore pari a `Wager + Bond`.
- **Effetto**: Viene creato un nuovo match con ID incrementale. Lo stato è `OPEN`.

### 2. Partecipazione (`joinMatch`)
- **Chi**: Player B.
- **Azione**: Invia una transazione con valore pari a `Wager + Bond` specificando l'ID del match.
- **Requisiti**: Il match deve essere `OPEN`.
- **Effetto**: Lo stato passa a `LOCKED`. I fondi totali nel contratto sono ora `2 * (Wager + Bond)`.

### 3. Sottomissione Risultati (`submitResult`)
- **Chi**: Entrambi i giocatori.
- **Azione**: Dichiarano `WIN` o `LOSS`.
- **Logica**:
    - Se Player A dice `WIN` e Player B dice `LOSS` (o viceversa), c'è accordo. Il vincitore viene pagato immediatamente.
    - Se entrambi dicono `WIN` o entrambi `LOSS`, c'è disaccordo. Lo stato passa a `DISPUTE_L1`.

### 4. Pagamento in caso di Accordo (`_payoutWinner`)
- **Vincitore**: Riceve `(2 * Wager) + Bond`. (La sua scommessa raddoppiata + la sua cauzione).
- **Perdente**: Riceve `Bond`. (La sua cauzione gli viene restituita perché è stato onesto nell'ammettere la sconfitta).

## Sistema di Dispute

### Innesco Disputa
Una disputa inizia se:
1. I giocatori inviano risultati contrastanti (es. entrambi dicono "Ho vinto").
2. Un giocatore reclama la vittoria per timeout dell'avversario (`claimTimeoutVictory`).

### Voto della Giuria (`juryVote`)
- **Chi**: Indirizzi presenti nella lista `mockJurors`.
- **Azione**: Votano per il vincitore reale basandosi sulle prove (off-chain).
- **Risoluzione**: Se una maggioranza vota per un giocatore, quel giocatore viene dichiarato vincitore tramite `_resolveDispute`.

### Risoluzione Disputa (`_resolveDispute`)
- **Vincitore**: Riceve l'intero piatto: `(2 * Wager) + (2 * Bond)`.
- **Perdente (Cheater)**: Perde tutto, inclusa la cauzione, che va al vincitore come risarcimento.

### Risoluzione Amministrativa (`adminResolveDispute`)
- **Chi**: L'owner del contratto (Super Admin).
- **Funzione**: Può forzare la risoluzione di una disputa.
- **Payout**:
    - **Vincitore**: Riceve `(2 * Wager) + Bond`.
    - **Admin**: Trattiene il `Bond` del perdente come tassa di servizio/punizione.

## Meccanismi di Sicurezza

### Timeout (`claimTimeoutVictory`)
Se un giocatore non risponde entro `TIMEOUT_WINDOW` (5 minuti), l'altro giocatore può reclamare la vittoria. Questo non assegna subito la vittoria ma apre una disputa per verifica.

### Annullamento (`withdrawPreMatch` / `voteToCancel`)
- **Pre-Match**: Se nessuno è entrato, Player A può ritirarsi e riprendere i fondi.
- **Post-Match**: Se il match è bloccato, entrambi i giocatori possono votare per annullare. Se entrambi votano sì, il match è `CANCELLED` e tutti vengono rimborsati.

## Configurazione
- **TIMEOUT_WINDOW**: 5 minuti.
- **JURY_SIZE**: 3 giurati (configurabile).
