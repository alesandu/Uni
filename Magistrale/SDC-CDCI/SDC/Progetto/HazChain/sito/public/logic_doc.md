# Documentazione di `logic.js` e `server/index.js`

## Architettura generale

```
 Browser
   │
   │  fetch POST /api/proxy  (o /api/login, /api/register)
   ▼
 server/index.js   ← Express.js su porta 3000
   │
   │  axios POST http://localhost:9999/api
   ▼
 Blockchain API (Hyperledger Fabric chaincode)
```

- **`logic.js`** — frontend (browser): costruisce i payload, valida le operazioni, gestisce la UI.
- **`server/index.js`** — backend (Node.js): fa da proxy tra browser e chaincode, gestisce autenticazione JWT e serve i file statici.

Il flusso logistico dei package è:

```
LAB → VAN (F) → HUB → TRUCK (C) → LANDFILL
```

---

## Indice — `logic.js`

1. [Blocco di avvio – Auth Check](#1-blocco-di-avvio--auth-check)
2. [callBlockchain](#2-callblockchain)
3. [fetchPackageState](#3-fetchpackagestate)
4. [fetchTransferState](#4-fetchtransferstate)
5. [fetchActorState](#5-fetchactorstate)
6. [fetchActorOptions](#6-fetchactoroptions)
7. [validateTransferIntegrity](#7-validatetransferintegrity)
8. [validateRiskCodeCompatibility](#8-validateriskcodecompatibility)
9. [checkSequence](#9-checksequence)
10. [validateVehicleCapacity](#10-validatevehiclecapacity)
11. [validateUserRole](#11-validateuserrole)
12. [Listener: btn-execute-builder](#12-listener-btn-execute-builder)
13. [generateNextId](#13-generatenextid)
14. [Listener: btn-p1-create – Crea Package](#14-listener-btn-p1-create--crea-package)
15. [Listener: btn-p1-start – Request Pickup (Phase 1)](#15-listener-btn-p1-start--request-pickup-phase-1)
16. [Listener: btn-p1-pickup – Transport Pickup (Phase 1)](#16-listener-btn-p1-pickup--transport-pickup-phase-1)
17. [Listener: btn-p1-arrive – Arrive at Hub (Phase 1)](#17-listener-btn-p1-arrive--arrive-at-hub-phase-1)
18. [Listener: btn-p2-start – Request Haz Pickup (Phase 2)](#18-listener-btn-p2-start--request-haz-pickup-phase-2)
19. [Listener: btn-p2-pickup – Haz Pickup (Phase 2)](#19-listener-btn-p2-pickup--haz-pickup-phase-2)
20. [Listener: btn-p2-arrive – Arrive at Landfill (Phase 2)](#20-listener-btn-p2-arrive--arrive-at-landfill-phase-2)
21. [Listener: btn-p2-dispose – Dispose Package](#21-listener-btn-p2-dispose--dispose-package)
22. [Listener: btn-inspect – Inspector/Search](#22-listener-btn-inspect--inspectorsearch)
23. [fetchAlerts](#23-fetchalerts)
24. [dismissAlert](#24-dismissalert)
25. [Listener: btn-refresh-alerts](#25-listener-btn-refresh-alerts)
26. [Polling alert automatico](#26-polling-alert-automatico)
27. [Redirect login automatico](#27-redirect-login-automatico)
28. [switchTab](#28-switchtab)
29. [clearMessages](#29-clearmessages)
30. [showError](#30-showerror)
31. [showSuccess](#31-showsuccess)
32. [handleLogin](#32-handlelogin)
33. [handleRegister](#33-handleregister)

---

## 1. Blocco di avvio – Auth Check

**Posizione:** righe 1–36

### Cosa fa
All'avvio dello script controlla se l'utente è autenticato:
- Legge `token` e `user` da `localStorage`.
- Se mancano, reindirizza a `login.html` (a meno di non essere già lì).
- Se presenti, parsa il JSON dell'utente e lo memorizza in `currentUser`.
- Aggiunge un listener su `DOMContentLoaded` per inserire il bottone **Logout** nella navbar, con username corrente.
- In caso di errore nel parsing del JSON, pulisce `localStorage` e reindirizza al login.

### Logica
Il meccanismo è un semplice guard di sessione client-side: senza token non si naviga. Il logout cancella entrambe le chiavi e redirige.

---

## 2. `callBlockchain`

```js
async function callBlockchain(payload, actionName)
```

### Cosa fa
Funzione centrale di comunicazione: invia una richiesta `POST` all'endpoint proxy (`http://localhost:3000/api/proxy`) con un payload JSON e restituisce la risposta.

### Parametri
| Param | Tipo | Descrizione |
|-------|------|-------------|
| `payload` | `Object` | Oggetto con `cmd`, `class`, `key`, `value` da mandare al chaincode |
| `actionName` | `string` | Etichetta descrittiva per i log |

### Logica
1. Logga l'inizio dell'operazione.
2. Esegue `fetch` al proxy.
3. Se la risposta HTTP è OK → logga il successo e restituisce `data`.
4. Se la risposta è in errore → logga i dettagli e restituisce `null`.
5. In caso di errore di rete → logga e restituisce `null`.

### Ritorna
`Object` con la risposta del chaincode, oppure `null` in caso di errore.

---

## 3. `fetchPackageState`

```js
async function fetchPackageState(packageId)
```

### Cosa fa
Recupera lo stato corrente di un **PACKAGE** dalla blockchain tramite `GetKV`.

### Parametri
| Param | Tipo | Descrizione |
|-------|------|-------------|
| `packageId` | `string` | ID del package (es. `PKG-2026-0001`) |

### Logica
Costruisce il payload `{ cmd: "GetKV", class: "PACKAGE", key: ["PKG", packageId] }` e lo invia a `callBlockchain`. Restituisce `data.answer.value` oppure `null`.

---

## 4. `fetchTransferState`

```js
async function fetchTransferState(packageId, transferId)
```

### Cosa fa
Recupera lo stato corrente di un **TRANSFER** dalla blockchain.

### Nota tecnica
La chiave corretta è `["TR", transferId]`, non include il `packageId` (il parametro `packageId` è usato solo per il log).

### Ritorna
`data.answer.value` oppure `null`.

---

## 5. `fetchActorState`

```js
async function fetchActorState(actorId)
```

### Cosa fa
Recupera lo stato corrente di un **ACTOR** (LAB, VAN, HUB, TRUCK, LANDFILL) dalla blockchain tramite `GetKV`.

### Ritorna
`data.answer.value` oppure `null`.

---

## 6. `fetchActorOptions`

```js
async function fetchActorOptions(filterStr)
```

### Cosa fa
Recupera tutti gli attori dalla blockchain e filtra quelli il cui ID inizia con `filterStr`. Usata per popolare i dropdown del builder (es. `"F"` → VAN, `"HUB"` → Hub, `"C"` → TRUCK, `"LANDFILL"` → Landfill).

### Logica
1. Ottiene tutte le chiavi di classe ACTOR con `GetKeys`.
2. Filtra le chiavi che iniziano con `filterStr`.
3. Se non ne trova, usa come fallback `${filterStr}-01`.

### Ritorna
`Array<string>` con gli ID degli attori filtrati.

---

## 7. `validateTransferIntegrity`

```js
async function validateTransferIntegrity(payload)
```

### Cosa fa
Valida che i campi critici di un **TRANSFER** (`weightKg`, `wasteType`, `riskCode`) corrispondano a quelli del **PACKAGE** associato. Blocca il trasferimento se ci sono discrepanze.

### Logica
1. Controlla che `payload.class === "TRANSFER"` e che sia presente un `packageId`.
2. Recupera lo stato del PACKAGE.
3. Confronta numericamente `weightKg`, come stringa `wasteType`, come array ordinato `riskCode`.
4. Se tutti i campi corrispondono → `{ valid: true }`.
5. Se ci sono discrepanze:
   - Genera un ID alert con `generateNextId`.
   - Scrive un record `INTEGRITY_MISMATCH` sulla blockchain.
   - Restituisce `{ valid: false, reason: "..." }`.

---

## 8. `validateRiskCodeCompatibility`

```js
async function validateRiskCodeCompatibility(payload)
```

### Cosa fa
Verifica che l'attore destinazione di un TRANSFER gestisca almeno uno dei `riskCode` del package. Blocca il trasferimento se l'intersezione è vuota.

### Logica
1. Recupera il package e i suoi `riskCode`.
2. Recupera l'attore destinazione e i suoi `riskCodesHandled`.
3. Calcola l'intersezione.
4. Se l'intersezione è vuota:
   - Crea un alert `RISK_CODE_INCOMPATIBLE` sulla blockchain.
   - Restituisce `{ valid: false, reason: "..." }`.

---

## 9. `checkSequence`

```js
async function checkSequence(pkgId, pkgState, requiredStates, actionLabel)
```

### Cosa fa
Controlla che il package si trovi in uno degli stati ammessi (`requiredStates`) prima di consentire un'azione. Viene chiamata **prima** di popolare il form del builder, così il blocco è immediato.

### Parametri
| Param | Tipo | Descrizione |
|-------|------|-------------|
| `pkgId` | `string` | ID del package |
| `pkgState` | `Object` | Stato corrente del package (già recuperato) |
| `requiredStates` | `string[]` | Stati ammessi per l'operazione |
| `actionLabel` | `string` | Nome dell'azione tentata (per log e alert) |

### Logica
- Se `pkgState` è assente o senza `.state` → permette (non può validare).
- Se `currentState` è in `requiredStates` → restituisce `true` (OK).
- Altrimenti crea un alert `SEQUENCE_VIOLATION` sulla blockchain e restituisce `false`.

---

## 10. `validateVehicleCapacity`

```js
async function validateVehicleCapacity(payload)
```

### Cosa fa
Blocca un PICKUP (`status: "SHIPPING"`) se il veicolo trasportatore supererebbe la propria `capacityKg` aggiungendo il peso del package.

### Logica
1. Controlla che lo status sia `"SHIPPING"`.
2. Legge il trasportatore dal campo `from`.
3. Recupera lo stato dell'attore e verifica che sia `TRANSPORT_LIGHT` o `TRANSPORT_HAZ`.
4. Calcola: `newLoad = transportedKg + packageWeight`.
5. Se `newLoad > capacityKg`:
   - Crea un alert `CAPACITY_EXCEEDED`.
   - Restituisce `{ valid: false }`.

---

## 11. `validateUserRole`

```js
async function validateUserRole(payload)
```

### Cosa fa
Verifica che l'utente loggato abbia il ruolo corretto per eseguire l'operazione indicata nel payload.

### Logica
- `SUPERUSER` bypassa sempre.
- `ACTOR` → solo `SUPERUSER`.
- `PACKAGE` con `state: IN_LAB` → richiede `LAB`.
- `PACKAGE` con `state: DISPOSED` → richiede `LANDFILL`.
- `TRANSFER` in `WAITING` con prefix `TR-F` → richiede `LAB`; `TR-C` → richiede `HUB`.
- `TRANSFER` in `SHIPPING/COMPLETED` con prefix `TR-F` → richiede `TRANSPORT_LIGHT`; `TR-C` → richiede `TRANSPORT_HAZ`.

In caso di ruolo non corrispondente, crea un alert `UNAUTHORIZED_ACTION` sulla blockchain e restituisce `{ valid: false }`.

---

## 12. Listener: `btn-execute-builder`

**Evento:** `click`

### Cosa fa
Orchestratore principale dell'esecuzione: applica nella sequenza corretta tutte le validazioni e poi invia il payload alla blockchain.

### Pipeline di esecuzione
1. **Role Validation** – `validateUserRole`
2. **Smart Merge** – per `PACKAGE`, `ACTOR`, `TRANSFER`: recupera i dati esistenti e li unisce con i nuovi (i campi vuoti nel builder non sovrascrivono quelli esistenti).
3. **Integrity Check su TRANSFER** – `validateTransferIntegrity`
4. **Integrity Check su PACKAGE** – confronta `weightKg`, `wasteType`, `riskCode` con i dati già salvati.
5. **Risk Code Compatibility** – `validateRiskCodeCompatibility`
6. **Vehicle Capacity** – `validateVehicleCapacity`
7. **Duplicate Entity Check** – verifica che PACKAGE e ACTOR con stesso ID non esistano già.
8. **Esecuzione** – `callBlockchain(payload, ...)`.
9. **Auto-Sync** – Dopo ogni TRANSFER, aggiorna automaticamente lo stato del PACKAGE associato:
   - `WAITING` → stato package invariato.
   - `SHIPPING` → `currentCustodian = from`, stato → `F_TRANSPORT` o `C_TRANSPORT`, incrementa `transportedKg` sul veicolo.
   - `COMPLETED` → `currentCustodian = to`, stato → `IN_HUB` o `IN_LANDFILL`, decrementa `transportedKg` sul veicolo.

---

## 13. `generateNextId`

```js
async function generateNextId(className, bucket, prefix)
```

### Cosa fa
Genera un ID univoco e progressivo per una nuova entità (PACKAGE, TRANSFER, ALERT) leggendo tutte le chiavi esistenti dalla blockchain.

### Parametri
| Param | Tipo | Esempio |
|-------|------|---------|
| `className` | `string` | `"PACKAGE"`, `"TRANSFER"`, `"ALERT"` |
| `bucket` | `string` | `"PKG"`, `"TR"`, `"AL"` |
| `prefix` | `string` | `"PKG-2026-"`, `"TR-F-"`, `"AL-"` |

### Logica
1. Ottiene tutte le chiavi esistenti della classe.
2. Filtra quelle con il `prefix` dato.
3. Estrae la parte numerica, trova il massimo.
4. Restituisce `prefix + (max+1)` con zero-padding a 4 cifre (es. `PKG-2026-0003`).

---

## 14. Listener: `btn-p1-create` – Crea Package

**Evento:** `click`

### Cosa fa
Prepara il builder per creare un nuovo PACKAGE in laboratorio.

### Logica
1. Genera un nuovo ID con `generateNextId("PACKAGE", "PKG", "PKG-2026-")`.
2. Recupera gli attori LAB dalla blockchain per popolare il dropdown `createdBy`.
3. Imposta i campi visibili: `createdBy` (select), `riskCode` (checkbox), `wasteType`, `weightKg`.
4. Imposta i campi nascosti: `state: "IN_LAB"`, `currentCustodian: ""`, `lastTransferId: "null"`, `lastUpdateTs`.

---

## 15. Listener: `btn-p1-start` – Request Pickup (Phase 1)

**Evento:** `click`

### Cosa fa
Prepara il builder per **richiedere** il ritiro del package da parte di un VAN (TRANSFER in WAITING, Fase 1).

### Logica
1. Ottiene gli attori VAN (`fetchActorOptions("F")`).
2. Recupera l'ID del package dal builder o chiede all'utente (`requestPackageId`).
3. **Sequence Check**: il package deve essere `IN_LAB`.
4. Determina `fromActor` → `currentCustodian` → `createdBy` → `"LAB-01"`.
5. Genera un nuovo Transfer ID (`TR-F-xxxx`).
6. Popola il builder con `status: "WAITING"`, `action: "PICKUP"`.

---

## 16. Listener: `btn-p1-pickup` – Transport Pickup (Phase 1)

**Evento:** `click`

### Cosa fa
Prepara il builder per **eseguire** il ritiro fisico dal laboratorio da parte del VAN (TRANSFER in SHIPPING, Fase 1).

### Logica
1. Recupera l'ID del package.
2. **Sequence Check**: il package deve essere `IN_LAB`.
3. **Transfer Prefix Check**: `lastTransferId` deve iniziare con `TR-F-` (deve esistere una richiesta previa).
4. Risolve `fromActor` (il VAN assegnato) con priorità:
   1. Campo `to` del TRANSFER in WAITING recuperato dalla blockchain.
   2. Campo `to` ancora visibile nel builder form.
   3. Fallback hardcoded `"F-01"`.
5. Popola il builder con `status: "SHIPPING"`, `action: "SHIPPING"`, destinazione HUB.

---

## 17. Listener: `btn-p1-arrive` – Arrive at Hub (Phase 1)

**Evento:** `click`

### Cosa fa
Prepara il builder per segnare l'**arrivo del package all'HUB** (TRANSFER in COMPLETED, Fase 1).

### Logica
1. Recupera l'ID del package.
2. **Sequence Check**: il package deve essere `F_TRANSPORT`.
3. Recupera i dati del TRANSFER dal `lastTransferId`.
4. Sovrascrive `status: "COMPLETED"`, `action: "DELIVER"` mantenendo tutti gli altri attributi del transfer.
5. Se il fetch del transfer fallisce, costruisce un form di fallback con HUB come destinazione.

---

## 18. Listener: `btn-p2-start` – Request Haz Pickup (Phase 2)

**Evento:** `click`

### Cosa fa
Prepara il builder per **richiedere** il ritiro da HUB da parte di un TRUCK (TRANSFER in WAITING, Fase 2).

### Logica
Analogo a `btn-p1-start` ma:
- Cerca attori `"C"` (TRUCK).
- **Sequence Check**: il package deve essere `IN_HUB`.
- Il `fromActor` è il `currentCustodian` (l'HUB).
- Genera un nuovo Transfer ID (`TR-C-xxxx`).

---

## 19. Listener: `btn-p2-pickup` – Haz Pickup (Phase 2)

**Evento:** `click`

### Cosa fa
Prepara il builder per **eseguire** il ritiro dall'HUB da parte del TRUCK (TRANSFER in SHIPPING, Fase 2).

### Logica
Analogo a `btn-p1-pickup` ma:
- **Sequence Check**: il package deve essere `IN_HUB`.
- **Transfer Prefix Check**: `lastTransferId` deve iniziare con `TR-C-`.
- `fromActor` è risolto leggendo `transfer.to` (il TRUCK assegnato nella fase WAITING precedente).
- Fallback: `"C-01"`.
- Destinazione: LANDFILL.

---

## 20. Listener: `btn-p2-arrive` – Arrive at Landfill (Phase 2)

**Evento:** `click`

### Cosa fa
Prepara il builder per segnare l'**arrivo del package alla LANDFILL** (TRANSFER in COMPLETED, Fase 2).

### Logica
Analogo a `btn-p1-arrive` ma:
- **Sequence Check**: il package deve essere `C_TRANSPORT`.
- Destinazione: LANDFILL.

---

## 21. Listener: `btn-p2-dispose` – Dispose Package

**Evento:** `click`

### Cosa fa
Prepara il builder per **smaltire** definitivamente il package (imposta `state: "DISPOSED"`).

### Logica
1. Recupera l'ID del package.
2. **Sequence Check**: il package deve essere `IN_LANDFILL`.
3. Recupera lo stato attuale del package e pre-compila il builder con tutti i suoi campi.
4. Sovrascrive `state: "DISPOSED"` e `lastUpdateTs`.
5. Se il fetch fallisce, mostra un form vuoto (lo Smart Merge nell'esecuzione compenserà).

---

## 22. Listener: `btn-inspect` – Inspector/Search

**Evento:** `click`

### Cosa fa
Esegue una ricerca tra i PACKAGE sulla blockchain applicando filtri lato client.

### Logica
1. **Strategia 1 – ID diretto:** se è inserito un `packageId`, recupera solo quel package.
2. **Strategia 2 – Scan completo:** scarica tutti i package (`GetKeys`) e poi i loro dettagli in parallelo (`Promise.all`).
3. Applica i filtri client-side:
   - `createdBy`, `currentCustodian` (contains case-insensitive)
   - `state/status` (exact match case-insensitive)
   - `wasteType` (contains)
   - `riskCode` (contains, supporta array e stringa)
   - `weightKg` (range min/max)
   - `lastTransferId` (contains)
4. Renderizza i risultati con `renderPackageCard`.

---

## 23. `fetchAlerts`

```js
async function fetchAlerts()
```

### Cosa fa
Recupera tutti gli alert dalla blockchain e li renderizza nel pannello `alerts-container`.

### Logica
1. Se l'elemento `alerts-container` non esiste nella pagina, esce subito.
2. Mostra "Loading..." solo al primo caricamento.
3. Recupera tutte le chiavi `ALERT` con `GetKeys`.
4. Recupera i dettagli di ogni alert in parallelo.
5. Filtra gli alert con `dismissed: true`.
6. Chiama `renderAlerts(alerts, container)`.

---

## 24. `dismissAlert`

```js
async function dismissAlert(alertId)
```

### Cosa fa
**Elimina fisicamente** un alert dalla blockchain usando il comando `DelKV`, quindi lo rimuove anche dall'UI con un'animazione.

### Logica
1. Invia `DelKV` alla blockchain per cancellare definitivamente l'alert:
   ```json
   { "cmd": "DelKV", "class": "ALERT", "key": ["AL", "<alertId>"] }
   ```
2. Anima la rimozione della card dall'UI (opacity/transform transition + `remove()` dopo 300ms).
3. Dopo 400ms richiama `fetchAlerts()` per aggiornare la lista.

> [!NOTE]
> Il record viene rimosso dalla blockchain in modo permanente (hard delete), a differenza del precedente approccio soft-delete che aggiungeva solo un flag `dismissed: true`.

---

## 25. Listener: `btn-refresh-alerts`

**Evento:** `click`

Richiama manualmente `fetchAlerts()`.

---

## 26. Polling alert automatico

```js
if (window.location.pathname.endsWith("alerts.html")) { ... }
```

### Cosa fa
Se la pagina corrente è `alerts.html`:
- Esegue `fetchAlerts()` immediatamente al caricamento.
- Poi ogni **5 secondi** tramite `setInterval`.

---

## 27. Redirect login automatico

```js
if (window.location.pathname.endsWith('login.html')) { ... }
```

### Cosa fa
Se l'utente è già loggato (token + user validi in `localStorage`) e naviga su `login.html`, viene reindirizzato automaticamente alla pagina del proprio ruolo senza dover reinserire le credenziali.

### Mappa ruolo → pagina
| Ruolo | Pagina |
|-------|--------|
| LAB | `lab.html` |
| TRANSPORT_LIGHT | `van.html` |
| HUB | `hub.html` |
| TRANSPORT_HAZ | `truck.html` |
| LANDFILL | `landfill.html` |
| SUPERUSER | `alerts.html` |

---

## 28. `switchTab`

```js
function switchTab(tab)
```

### Cosa fa
Gestisce il cambio tra i tab **Login** e **Register** nella pagina di login.

### Logica
Rimuove la classe `active` da entrambi i tab e form, poi aggiunge `active` solo al tab e al form passati come argomento. Chiama anche `clearMessages()`.

---

## 29. `clearMessages`

```js
function clearMessages()
```

### Cosa fa
Nasconde sia il box errore che il box successo nella pagina di login.

---

## 30. `showError`

```js
function showError(msg)
```

### Cosa fa
Mostra un messaggio di errore nella pagina di login, nascondendo l'eventuale messaggio di successo.

---

## 31. `showSuccess`

```js
function showSuccess(msg)
```

### Cosa fa
Mostra un messaggio di successo nella pagina di login, nascondendo l'eventuale messaggio di errore.

---

## 32. `handleLogin`

```js
async function handleLogin(e)
```

### Cosa fa
Gestisce il submit del form di login.

### Logica
1. Previene il comportamento default del form (`e.preventDefault()`).
2. Disabilita il bottone e cambia il testo in `"Authenticating..."`.
3. Invia `POST /api/login` con `username` e `password`.
4. Se il server risponde OK:
   - Salva `token` e `user` in `localStorage`.
   - Reindirizza (con 500ms di delay) alla pagina del ruolo dell'utente.
5. In caso di errore → mostra il messaggio di errore e riabilita il bottone.
6. In caso di errore di rete → messaggio specifico.

---

## 33. `handleRegister`

```js
async function handleRegister(e)
```

### Cosa fa
Gestisce il submit del form di registrazione.

### Logica
1. Previene il comportamento default.
2. Disabilita il bottone e cambia il testo.
3. Invia `POST /api/register` con `username`, `password`, `role`.
4. Se OK:
   - Mostra messaggio di successo.
   - Dopo 2 secondi, passa automaticamente al tab Login e pre-compila il campo `username`.
5. In caso di errore → mostra il messaggio e riabilita il bottone.

---

---

# Documentazione di `server/index.js`

File del **backend Node.js/Express**. Gira sulla porta **3000**, serve i file statici del frontend e espone tre gruppi di endpoint:
1. `/api/proxy` — proxy verso il chaincode Fabric.
2. `/api/register` + `/api/login` — autenticazione utenti con JWT.

### Dipendenze principali
| Modulo | Uso |
|--------|-----|
| `express` | Server HTTP |
| `cors` | Permette richieste cross-origin dal browser |
| `axios` | Chiama la Blockchain API su `localhost:9999` |
| `bcrypt` | Hash/verifica password |
| `jsonwebtoken` | Generazione e firma JWT |
| `db` | Modulo SQLite locale (`./db`) per gli utenti |

---

## 34. `getPackage` (helper interno)

```js
async function getPackage(packageId)
```

### Cosa fa
Recupera lo stato corrente di un PACKAGE direttamente dalla Blockchain API (non tramite proxy), ad uso interno del server.

### Logica
1. Costruisce il payload `{ cmd: "GetKV", class: "PACKAGE", key: ["PKG", packageId] }`.
2. Invia `POST http://localhost:9999/api` via `axios`.
3. Estrae il valore da `data.answer.value` (strategia principale) o da `data.value` (fallback).
4. Restituisce l'oggetto package oppure `null` in caso di errore.

### Nota
Usata solo internamente da `/api/proxy` per l'auto-sync server-side del package.

---

## 35. `POST /api/proxy`

```
POST /api/proxy
Body: payload JSON { cmd, class, key, value }
```

### Cosa fa
Proxy trasparente tra il browser e la Blockchain API. Inoltra qualsiasi payload e, in caso di operazione `AddKV` su classe `TRANSFER`, esegue un **auto-aggiornamento server-side** del PACKAGE associato.

### Logica
1. Riceve il payload dal browser.
2. Invia `POST http://localhost:9999/api` tramite `axios`.
3. Se la risposta è `200` **e** il comando è `AddKV` su `TRANSFER`:
   a. Estrae `transferId` da `key[1]` e `packageId` da `value.packageId`.
   b. Recupera lo stato attuale del package con `getPackage(packageId)`.
   c. Determina il nuovo `currentCustodian` e `state` in base allo `status` del transfer:

| Status transfer | Nuovo custodian | Nuovo stato package |
|-----------------|-----------------|---------------------|
| `WAITING` | `transfer.from` | invariato |
| `SHIPPING` (→ HUB) | `transfer.from` | `F_TRANSPORT` |
| `SHIPPING` (→ LANDFILL) | `transfer.from` | `C_TRANSPORT` |
| `COMPLETED`/`DELIVERED` (→ HUB) | `transfer.to` | `IN_HUB` |
| `COMPLETED`/`DELIVERED` (→ LANDFILL) | `transfer.to` | `IN_LANDFILL` |

   d. Aggiorna anche `lastTransferId` e `lastUpdateTs`.
   e. Se il transfer riporta `weightKg`, `wasteType` o `riskCode`, li copia nel package.
   f. Invia `AddKV PACKAGE` alla blockchain con i dati aggiornati.
4. Risponde al browser con lo stesso status/body ricevuto dalla blockchain.
5. In caso di errore: risponde con lo status dell'errore o `500`.

### Nota
Questo auto-sync **server-side** e quello in `logic.js` (**client-side**) sono ridondanti: entrambi tentano di aggiornare il package dopo una TRANSFER. In produzione converrebbe mantenerne uno solo.

---

## 36. `POST /api/register`

```
POST /api/register
Body: { username, password, role }
```

### Cosa fa
Registra un nuovo utente nel database SQLite locale.

### Logica
1. Valida la presenza di `username`, `password`, `role`.
2. Controlla nel DB se lo username esiste già (`SELECT id FROM users WHERE username = ?`).
3. Hasha la password con `bcrypt` (10 salt rounds).
4. Inserisce il nuovo utente (`INSERT INTO users ...`).
5. Risponde `201` con `{ message, userId }` oppure `400`/`500` in caso di errore.

### Sicurezza
- La password non è mai salvata in chiaro.
- Non c'è validazione del `role` lato server: qualsiasi stringa viene accettata.

---

## 37. `POST /api/login`

```
POST /api/login
Body: { username, password }
```

### Cosa fa
Autentica un utente e restituisce un **JWT** valido 24 ore.

### Logica
1. Valida la presenza di `username` e `password`.
2. Cerca l'utente nel DB (`SELECT * FROM users WHERE username = ?`).
3. Confronta la password con l'hash salvato tramite `bcrypt.compare`.
4. Se corretta, genera un JWT con `jwt.sign` (payload: `{ id, username, role }`, expire: `24h`).
5. Risponde con `{ message, token, user: { id, username, role } }`.
6. In caso di credenziali errate → `401 Invalid username or password`.

### Token JWT
Firmato con la costante `JWT_SECRET = "hazchain_super_secret_key_123"`. In produzione dovrebbe essere una variabile d'ambiente.

---

## 38. Avvio del server

```js
app.listen(3000, ...)
```

Avvia Express sulla porta **3000**. Serve anche i file statici dalla cartella `../public` (le pagine HTML, CSS e `logic.js` del frontend) tramite `express.static`.
