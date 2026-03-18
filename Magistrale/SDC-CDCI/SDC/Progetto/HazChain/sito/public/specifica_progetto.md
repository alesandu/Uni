# Specifica del Progetto HazChain

## 1. Introduzione
**HazChain** è una piattaforma digitale integrata, supportata da tecnologia DLT (Distributed Ledger Technology / Blockchain privata tramite Hyperledger Fabric), ideata per il tracciamento sicuro, trasparente e immutabile dei rifiuti pericolosi lungo tutta la filiera logistica. L'obiettivo primario del progetto è prevenire lo smaltimento illecito, verificare in tempo reale l'integrità del flusso di materiali e garantire il rispetto delle normative (compatibilità, licenze, carichi e passaggi autorizzati).

---

## 2. Architettura Tecnica

Il progetto è suddiviso in un frontend leggero e un backend/proxy per l'interazione con la rete DLT:
- **Frontend (Browser-side):** UI responsiva basata su HTML CSS vanilla e Vanilla JavaScript (`logic.js`, `ui.js`). La logica frontend implementa robuste validazioni *client-side* (in fase di builder delle richieste) pre-invio e si occupa di gestire dinamicamente lo stato dell'UI in base all'attore autenticato.
- **Backend Node.js (Proxy & Auth):** Sviluppato in Express (`server/index.js`). Svolge un duplice ruolo:
  1. Fornire un layer di autenticazione locale con SQLite per gli utenti della dashboard (login, registrazione) utilizzando un meccanismo basato su JWT (JSON Web Token).
  2. Implementare un Proxy API (rotte `POST /api/proxy`) verso il server API della Blockchain Fabric esposto alla porta `9999`. Il proxy implementa anche funzioni logiche server-side per l'autosincronizzazione dei `PACKAGE` ad ogni avvenuto `TRANSFER`.
- **Blockchain (Data Layer):** Network *Hyperledger Fabric* accessibile tramite chiamate API REST (comandi core: `AddKV`, `GetKV`, `DelKV`, `GetKeys`). Implementa uno strato documentale a chiave-valore per la persistenza di classi definite (`PACKAGE`, `TRANSFER`, `ACTOR`, `ALERT`).

---

## 3. Modello Dati e Strutture di Base

I dati inseriti sulla blockchain sono istanze delle seguenti *Classi (Class)*:

1. **`PACKAGE` (Pacco di Rifiuti):** Rappresenta il singolo asset fisico da tracciare.
   - **Campi chiave:** `packageId`, `createdBy`, `riskCode` (Array o Stringa), `wasteType`, `weightKg`, `state` (Stato formale), `currentCustodian` (Possessore corrente), `lastTransferId`, `lastUpdateTs`.

2. **`TRANSFER` (Operazione di Trasferimento):** Un log immutabile che traccia in maniera granulare il passaggio di consegne e gli eventi fisici.
   - **Campi chiave:** `transferId`, `from`, `to`, `action` (es. PICKUP, SHIPPING, DELIVER), `ts` (Timestamp), `status` (es. WAITING, SHIPPING, COMPLETED, CLOSED).

3. **`ACTOR` (Entità della Filiera):** Il profilo utente o il veicolo logistico.
   - **Campi chiave:** `actorId`, `role` (Ruolo aziendale/sistema), `name`, `location`/`company`, `capacityKg` (Peso massimo consentito - solo veicoli), `riskCodesHandled` (Lista di codici di rischio che la struttura o il veicolo è legalmente autorizzato a gestire), `transportedKg` (Carico in utilizzo corrente).

4. **`ALERT` (Log di Violazione):** Segnalazioni di mancata conformità di sicurezza, attivate e risolvibili (con *hard delete*) dal monitoraggio intelligente.

---

## 4. Gli Attori della Filiera

Il flusso dei materiali coinvolge 5 ruoli specifici (più uno di amministrazione generale) che interagiscono cronologicamente in successione:

1.  **LAB (Laboratorio/Produttore):** Genera fisicamente il `PACKAGE`. Responsabile dell'inizializzazione dell'asset nel sistema con attribuzione di peso e codice rischio. Chiama il trasportatore per il suo ritiro locale.
2.  **TRANSPORT_LIGHT (Furgone Locale - Logistica leggera):** Unità ritirante assegnata, che riceve in custodia il `PACKAGE` dal `LAB` per trasportarlo verso un polo intermedio. Sottoposto a controllo del carico veicolare massimo (`capacityKg`).
3.  **HUB (Centro di Smistamento):** Nodo regionale. Riceve i carichi vari dai colli di tipo `TRANSPORT_LIGHT`, li consolida temporaneamente e richiede pick-up specializzato.
4.  **TRANSPORT_HAZ (Camion Pesante/Rischi Elevati):** Mezzo ad elevata portata per logistiche terminali lunghe. Sottoposto ai medesimi rigidi vincoli di capacita del veicolo e controlli di compatibilità R-Codes. Preleva da `HUB` per la `LANDFILL`.
5.  **LANDFILL (Discarica Autorizzata - Destinazione finale):** Ente preposto alla distruzione/smaltimento della materia. Marca ufficialmente il materiale via comando apposito, dismettendo il ciclo-vita dell'asset con lo stato finale `DISPOSED`.
6.  **SUPERUSER:** Ruolo speciale con vista estesa (Monitoring Wall) e builder avanzato, preposto a ispezioni globali, risoluzione forzata di blocchi di rete o cancellazione (`dismiss`) di anomalie notificate (`ALERTS`).

---

## 5. Workflow Operativo (La Macchina a Stati)

Il sistema implementa severamente il ciclo vitale dei package seguendo una `State Machine` stretta, sia logica in blockchain, sia visualmente sul frontend per inibire azioni ai loggati non corrispondenti al ruolo della filiera richiesto.

| Azione Eseguita (Frontend) | Stato Asset Finale (`state`) | Custode Transitorio (`currentCustodian`) | Attore Abilitato (Role) |
|----------------------------|----------------------------|------------------------------------------|-------------------------|
| Creazione Asset originaria | `IN_LAB`                   | Autore/Laboratorio (`LAB-XX`)            | `LAB`                   |
| Ritiro (Pickup da Lab)     | `F_TRANSPORT`              | `TRANSPORT_LIGHT` (es `F-01`)             | `TRANSPORT_LIGHT`       |
| Arrivo all'Hub intermedio  | `IN_HUB`                   | Centro `HUB` (`HUB-XX`)                  | `HUB`                   |
| Ritiro (Pickup da Hub)     | `C_TRANSPORT`              | `TRANSPORT_HAZ` (es `C-01`)               | `TRANSPORT_HAZ`         |
| Arrivo in Discarica        | `IN_LANDFILL`              | Discarica (`LANDFILL-XX`)                  | `LANDFILL`              |
| Procedura di Smaltimento   | `DISPOSED`                 | Discarica (`LANDFILL-XX`)                  | `LANDFILL`              |

*Note sulla catena TRANSFER: I passaggi di consegne intermedi generano documenti documentali di classe `TRANSFER` i cui stati transizionali validi sono `WAITING` (In attesa operazione per il custode mittente) $\rightarrow$ `SHIPPING` (Sotto custodia di trasporto) $\rightarrow$ `COMPLETED` (Scaricati / Accettati in arrivo).*

---

## 6. Il Motore di Validazione e Sicurezza Automatica (Monitor Wall)

Le violazioni e le deviazioni standard dal workflow ottimale generano log immutabili di Sistema (`ALERT`) che prevengono l'aggiornamento critico scorretto (aborting):

*   **Sequence Integrity (`SEQUENCE_VIOLATION`):** Previsto che ogni transazione rispetti cronologicamente e semanticamente le istruzioni del grafo degli stati. Esempio limitativo: la discarica non può registrare "Arrivo" per un materiale `IN_LAB`.
*   **Data Integrity Check (`INTEGRITY_MISMATCH`):** Il sistema impedisce modifiche indebite e truffaldine durante i viaggi logistici. Il Peso corporeo (`weightKg`), tipologia e i codici in transito in un `TRANSFER` preposto debbono categoricamente essere comparabili pari all'equivalente del `PACKAGE`.
*   **Risk Code Compatibility Check (`RISK_CODE_INCOMPATIBLE`):** All'atto dell'affidamento di custodia, l'attore in ricezione o trasporto **deve** possedere obbligatoriamente il `riskCode` del materiale da caricare compreso nella sua lista di permessi operativi legali `riskCodesHandled`.
*   **Vehicle Capacity Constraint (`CAPACITY_EXCEEDED`):** Previsto sul monitoraggio logistico di mezzi Furgone e Camion. I payload bloccano i ritiri in rampa (Pickup `SHIPPING`) se il `weightKg` del carico in transazione va a sforare l'allocazione `capacityKg` limite indicata, tracciando i pesi totali attuali su attributo `transportedKg`.
*   **Role Constraint Authorization (`UNAUTHORIZED_ACTION`):** Blocchi client ed evizioni automatiche di invii payload con ruoli incompatibili (es. un autista Camion sprovvisto logicamente di autorizzazione per poter dismettere un pacco all'interno di un HUB).
*   **Entity Conflicts (`DUPLICATE_ENTITY`):** Il Builder convalida rigidamente i pool di chiavi in utilizzo per impedire che ID package logistici collidano, invalidando le riscritture incidentali di passate iterazioni operative.
