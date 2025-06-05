| Attore                | ID   | Requisiti Funzionali                         | Descrizione                                                                                  |
|-----------------------|------|----------------------------------------------|----------------------------------------------------------------------------------------------|
| Utente non registrato | 1.1  | Visualizzazione offerte e camere disponibili | Può consultare le camere disponibili, i servizi offerti e i prezzi senza registrazione.      |
|                       | 1.2  | Registrazione                                | Può registrarsi inserendo dati personali per diventare un cliente.                          |
| Cliente               | 2.1  | Effettua prenotazione                        | Può prenotare camere selezionando data di arrivo, partenza e tipo di stanza.                |
|                       | 2.2  | Gestione prenotazioni                        | Può visualizzare, modificare o cancellare le prenotazioni effettuate.                       |
|                       | 2.3  | Richiesta servizi extra                      | Può richiedere servizi come colazione in camera, lavanderia, spa, ecc.                      |
|                       | 2.4  | Pagamento online                             | Può effettuare il pagamento tramite carta di credito o altri metodi.                        |
| Utente registrato     | 3.1  | Effettua accesso                             | Può accedere al sistema inserendo le proprie credenziali.                                   |
|                       | 3.2  | Cancellazione account                        | Può cancellare il proprio account dal sistema.                                               |
|                       | 3.3  | Visualizza camere                            | Può visualizzare le camere disponibili nel sistema.                                          |
|                       | 3.4  | Aggiornamento dati personali                 | Può aggiornare i propri dati come nome, email, telefono, ecc.                               |
| Amministrazione       | 4.1  | Gestione prenotazioni clienti                | Può visualizzare, creare, modificare o cancellare prenotazioni per conto dei clienti.       |
|                       | 4.2  | Gestione check-in/check-out                  | Registra l'ingresso e l'uscita degli ospiti.                                                 |
|                       | 4.3  | Assegnazione camere                          | Assegna camere ai clienti in base a disponibilità e preferenze.                             |
|                       | 4.4  | Gestione reclami e richieste                 | Registra ed eventualmente smista le segnalazioni a chi di competenza.                       |
|                       | 4.5  | Gestione utenti del sistema                  | Può creare, modificare o disattivare utenti (receptionist, pulizie, ecc.).                  |
|                       | 4.6  | Monitoraggio generale attività               | Può consultare statistiche di occupazione, servizi usati, feedback clienti.                 |
| Servizi alberghieri   | 5.1  | Visualizza camere da pulire                  | Può vedere la lista delle camere da pulire ordinate per urgenza.                            |
|                       | 5.2  | Segnala completamento pulizia                | Può aggiornare lo stato della camera una volta pulita.                                      |
|                       | 5.3  | Gestione ordini dei clienti                  | Riceve richieste dei clienti per servizio in camera o in sala.                              |
|                       | 5.4  | Menù aggiornabile                            | Può aggiornare il menù giornaliero visibile ai clienti.                                     |

---

| ID   | Requisito Non Funzionale | Descrizione                                                                                      |
|------|---------------------------|--------------------------------------------------------------------------------------------------|
| NF1  | Usabilità                 | Il sistema deve essere semplice e intuitivo per ogni tipo di utente.                           |
| NF2  | Sicurezza                 | I dati personali e i pagamenti devono essere protetti con crittografia e autenticazione sicura.|
| NF3  | Disponibilità             | Il sistema deve essere disponibile 24/7, con eventuali manutenzioni notturne programmate.      |
| NF4  | Scalabilità               | Il sistema deve poter gestire un aumento del numero di clienti, camere e personale.            |
| NF5  | Compatibilità             | Deve essere accessibile da desktop e dispositivi mobili.                                       |
| NF6  | Prestazioni               | Le operazioni principali devono avvenire entro 2 secondi di latenza.                           |
| NF7  | Manutenibilità            | Il codice deve essere facilmente aggiornabile con una struttura modulare.                      |
| NF8  | Backup                    | Il sistema deve effettuare backup giornalieri automatici dei dati.                             |


# 1 Introduzione

**Sistema di Gestione Hotel**

Il software è progettato per semplificare e automatizzare la gestione operativa di una struttura alberghiera, riducendo il rischio di errori, ottimizzando i processi e migliorando l’esperienza degli ospiti. Il sistema supporta le attività quotidiane del personale, consentendo una gestione integrata delle prenotazioni, del check-in/check-out, della pulizia delle camere, della comunicazione interna tra il personale.

Gli utenti non registrati possono consultare camere disponibili, offerte e prezzi senza obbligo di registrazione. Se interessati, possono procedere alla registrazione inserendo i propri dati personali per accedere a servizi avanzati.

I Clienti sono utenti registrati possono effettuare prenotazioni online selezionando le date, la tipologia di camera e i servizi aggiuntivi desiderati. Le disponibilità vengono aggiornate in tempo reale e ogni prenotazione è automaticamente notificata al personale di ricevimento. Hanno la possibilità di gestire in autonomia le proprie prenotazioni (modifiche, cancellazioni), effettuare il **check-in e check-out online**, aggiornare i propri dati e concludere i pagamenti attraverso metodi elettronici integrati.

Il receptionist è un utente registrato che ha accesso a funzionalità avanzate per gestire le prenotazioni dei clienti, assegnare le camere in base alla disponibilità, registrare ingressi/uscite e gestire reclami o richieste speciali, smistandoli a chi di competenza.

Il personale addetto alle pulizie è un utente registrato che riceve notifiche giornaliere sulle camere da sistemare, e può consultare in tempo reale la lista delle camere da riordinare e aggiornare lo stato di pulizia al termine delle attività. 

Anche il servizio di ristorazione è un utente registrato con funzionalità avanzate che può gestire ordini ricevuti dai clienti, aggiornare il menù giornaliero e organizzare le consegne in camera o in sala.

L'amministratore dell’hotel ha accesso a un pannello di controllo che consente di monitorare in tempo reale: 
- l’occupazione delle camere, 
- la produttività del personale, 
- le recensioni degli ospiti.
Inoltre può creare, modificare o disattivare utenti appartenenti a diverse categorie (receptionist, pulizie, ristorazione)

Attraverso l’integrazione con sistemi di messaggistica (SMS, email, notifiche app), il software permette una comunicazione fluida con gli ospiti prima, durante e dopo il soggiorno, per inviare promemoria, suggerimenti personalizzati e offerte dedicate.

Tutte le operazioni sono tracciate e accessibili in un unico portale centralizzato, disponibile da desktop e dispositivi mobili. Il sistema è progettato per garantire sicurezza, flessibilità e scalabilità, ed è conforme alle normative vigenti in materia di trattamento dei dati personali, rispettando i requisiti normativi in materia di privacy.


# Glossario

| Termine                     | Definizione                                                                                   |
|-----------------------------|-----------------------------------------------------------------------------------------------|
| Utente non registrato       | Visitatori del sito o portale che non hanno ancora creato un account.                         |
| Cliente                     | Utente registrato che può effettuare prenotazioni, pagamenti e accedere a servizi personalizzati. |
| Utente registrato           | Qualsiasi utente che ha effettuato la registrazione e può accedere a funzionalità avanzate.   |
| Amministrazione         | Reparto o insieme di utenti (recptionist e amministratore) con privilegi elevati che sovrintende alle operazioni interne: gestione prenotazioni, assegnazione camere, gestione utenti e analisi statistiche. |
| Receptionist                | Membro dello staff incaricato della gestione delle prenotazioni, del check-in/check-out e del contatto diretto con i clienti. |
| Amministratore              | Figura con accesso completo al sistema, responsabile della gestione degli utenti e del monitoraggio generale; è il proprietario dell'hotel. |

| Servizi alberghieri     | Dipartimento operativo che eroga i servizi di pulizia, ristorazione e altri servizi extra richiesti dagli ospiti durante il soggiorno. |
| Servizio pulizie            | Personale responsabile dell'ordine e della pulizia delle camere.                              |
| Servizio ristorazione       | Personale addetto alla gestione degli ordini per pasti in camera o in sala.                   |
| Prenotazione                | Operazione con cui un cliente riserva una camera per un determinato periodo.                  |
| Check-in                    | Procedura di registrazione all’arrivo del cliente in struttura.                               |
| Check-out                   | Procedura di uscita del cliente al termine del soggiorno.                                     |
| Reclami e richieste         | Segnalazioni o domande inviate dai clienti relative al loro soggiorno.                        |
| Stato pulizia               | Indicatore aggiornato sullo stato di ordine e igiene di una camera.                           |
| Sistema                     | L'applicazione software per la gestione completa dell’hotel.                                  |
| Portale                     | Interfaccia web o app tramite cui gli utenti interagiscono con il sistema.                    |


associazioni come attributi quindi scegliere se non metterli, importante mettere unicità però (si possono fare entrambi i modi, metterli e non metterli)
errori tipici: design pattern che non si capiscono la sostanza del pattern, observer ad esempio bisogna ricordarsi tutto il workflow
inxlude extend
diagramma dei casi d uso non si capisce quale attore attiva cosa
verificabilità dei reqiusiti utenti (l interfaccia utente sia facile da usare NON METTERE QUESTO TIPO DI REQUISITI)
tutti i requisiti devono essere verificabili e consistente (non mettere opzionali)
conoscere l intero progetto

per dubbi contattarlo o chiedere ricevimento su teams

# **Caso d'Uso: Utente non Registrato**

### **Use Case 1.1 – Visualizzazione Offerte e Camere Disponibili**

| **Elemento**            | **Descrizione**                                                                                                                                                                                                        |
| ----------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **Passi Azione**        | 1. L’utente non registrato entra nel sistema.<br>2. Seleziona il periodo di tempo per cui desidera visualizzare le offerte.<br>3. Il sistema mostra le offerte e le camere disponibili con prezzi e servizi associati. |
| **Attori**              | Utente non registrato                                                                                                                                                                                                  |
| **Precondizioni**       | Nessuna precondizione particolare, l’utente deve solo accedere al sistema.                                                                                                                                             |
| **Scenario principale** | L’utente consulta le offerte e le camere disponibili senza autenticazione.                                                                                                                                             |
| **Scenari alternativi** | 3.1 La camera scelta da visualizzare non è disponibile nel periodo scelto.<br>3.2 Il sistema mostra opzioni alternative disponibili per quel periodo.                                                                  |
| **Post-condizioni**     | L’utente non registrato ha visionato le informazioni di cui aveva bisogno.                                                                                                                                             |

---

### **Use Case 1.2 – Registrazione**

| **Elemento**            | **Descrizione**                                                                                                                                                                                    |
| ----------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **Passi Azione**        | 1. L’utente accede alla sezione “Registrati” e inserisce le informazioni richieste (nome utente, password, email, ecc.).<br>2. Il sistema valida i dati e crea un nuovo profilo utente registrato. |
| **Attori**              | Utente non registrato                                                                                                                                                           |
| **Precondizioni**       | L’utente accede alla sezione “Registrati” del sistema.                                                                                                                                             |
| **Scenario principale** | L’utente si registra al sistema per accedere a funzionalità aggiuntive.                                                                                                                            |
| **Scenari alternativi** | 2.1 Il sistema rileva errori nei dati inseriti (es. dati non validi, email già in uso).<br>2.2 Il sistema mostra un messaggio di errore esplicativo e riporta l’utente al form.                    |
| **Post-condizioni**     | L’utente ha un nuovo account ed è ora considerato utente registrato.                                                                                                                               |

---

# **Casi d’uso: Utente Registrato**

---

### **Use Case 3.1 – Effettua accesso**

| **Elemento**            | **Descrizione**                                                                                                                                                                                                                       |
| ----------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **Passi Azione**        | 1. L’utente richiede la pagina principale.<br>2. Il sistema mostra la pagina di login.<br>3. L’utente inserisce email e password.<br>4. Il sistema verifica le credenziali.<br>5. Se corrette, l’utente accede al pannello personale. |
| **Attori**              | Utente registrato                                                                                                                                                                                                                     |
| **Precondizioni**       | L’utente deve essere registrato e avere credenziali valide.                                                                                                                                                                           |
| **Scenario principale** | L’utente inserisce correttamente le credenziali ed entra nel sistema.                                                                                                                                                                 |
| **Scenari alternativi** | 1. L’utente inserisce credenziali errate: il sistema mostra un messaggio d’errore.<br>2. L’utente ha dimenticato la password: può accedere alla funzione “Recupera password”.                                                         |
| **Post-condizioni**     | L’utente ha accesso alla propria area personale.                                                                                                                                                                                      |

---

### **Use Case 3.2 – Cancellazione account**

| **Elemento**            | **Descrizione**                                                                                                                                                                                                                                           |
| ----------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **Passi Azione**        | 1. L’utente accede alla sezione "Gestione Account".<br>2. Seleziona “Elimina Account”.<br>3. Il sistema chiede conferma.<br>4. L’utente conferma l’intenzione.<br>5. Il sistema elimina o disattiva l’account, rimuovendo i dati personali ove possibile. |
| **Attori**              | Utente registrato                                                                                                                                                                                                                                         |
| **Precondizioni**       | L’utente deve essere autenticato.                                                                                                                                                                                                                         |
| **Scenario principale** | L’utente conferma la volontà di cancellare il proprio account.                                                                                                                                                                                            |
| **Scenari alternativi** | 1. L’utente annulla l’operazione: l’account rimane attivo.                                                                                                                                                                                                |
| **Post-condizioni**     | L’account viene cancellato o disattivato.                                                                                                                                                                                                                 |

---

### **Use Case 3.3 – Visualizza camere**

| **Elemento**            | **Descrizione**                                                                                                                                                                                                                                          |
| ----------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **Passi Azione**        | 1. L’utente accede al sistema.<br>2. Seleziona la voce “Camere” o “Offerte disponibili”.<br>3. Il sistema mostra la lista delle camere disponibili, prezzi, descrizione e servizi inclusi.<br>4. L’utente può filtrare per data, prezzo, categoria, ecc. |
| **Attori**              | Utente registrato, Utente non registrato                                                                                                                                                                                                                 |
| **Precondizioni**       | L’utente deve visitare il portale                                                                                                                                                                                                                        |
| **Scenario principale** | L’utente esplora le camere disponibili con i relativi dettagli.                                                                                                                                                                                          |
| **Scenari alternativi** | 1. Il sistema non mostra camere se non disponibili per le date o i filtri selezionati.                                                                                                                                                                   |
| **Post-condizioni**     | L’utente ottiene informazioni aggiornate sulle camere.                                                                                                                                                                                                   |

---

### **Use Case 3.4 – Aggiornamento dati personali**

| **Elemento**            | **Descrizione**                                                                                                                                                                                                                                        |
| ----------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| **Passi Azione**        | 1. L’utente accede alla sezione "Profilo personale".<br>2. Seleziona il campo da modificare (nome, email, telefono, password, ecc.).<br>3. Inserisce i nuovi dati.<br>4. Conferma la modifica.<br>5. Il sistema aggiorna le informazioni nel database. |
| **Attori**              | Utente registrato                                                                                                                                                                                                                                      |
| **Precondizioni**       | L’utente deve essere autenticato.                                                                                                                                                                                                                      |
| **Scenario principale** | L’utente aggiorna correttamente uno o più dati del proprio profilo.                                                                                                                                                                                    |
| **Scenari alternativi** | 1. L’utente inserisce un formato non valido: il sistema mostra un errore.<br>2. L’utente lascia campi obbligatori vuoti: il sistema blocca la modifica.                                                                                                |
| **Post-condizioni**     | I dati personali dell’utente risultano aggiornati nel sistema.                                                                                                                                                                                         |

---

# **Casi d’uso: Cliente**

---

### **Use Case 2.1 – Effettua prenotazione**

| **Elemento**            | **Descrizione**                                                                                                                                                                                      |
| ----------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **Passi Azione**        | 1. Il cliente accede al sistema e seleziona le date di arrivo e partenza.<br>2. Il cliente seleziona la tipologia di camera desiderata.<br>3. Il cliente conferma i dati e completa la prenotazione. |
| **Attori**              | Cliente                                                                                                                                                                                              |
| **Precondizioni**       | Il cliente ha effettuato l'accesso al sistema.                                                                                                                                                       |
| **Scenario principale** | Il cliente effettua una prenotazione per un soggiorno.                                                                                                                                               |
| **Scenari alternativi** | La camera selezionata potrebbe non essere disponibile, viene suggerita un'alternativa.                                                                                                               |
| **Post-condizioni**     | La prenotazione è registrata nel sistema e visibile nella sezione personale del cliente.                                                                                                             |

---

### **Use Case 2.2 – Gestione prenotazioni**

| **Elemento**            | **Descrizione**                                                                                                                                                                                                          |
| ----------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| **Passi Azione**        | 1. Il cliente visualizza la lista delle prenotazioni effettuate.<br>2. Il cliente seleziona una prenotazione da modificare o cancellare.<br>3. Il sistema applica le modifiche richieste oppure elimina la prenotazione. |
| **Attori**              | Cliente, Amministrazione                                                                                                                                                                                                 |
| **Precondizioni**       | Il cliente ha effettuato almeno una prenotazione e ha effettuato l’accesso.                                                                                                                                              |
| **Scenario principale** | Il cliente modifica o cancella una prenotazione esistente.                                                                                                                                                               |
| **Scenari alternativi** | La prenotazione non è modificabile (es. troppo vicino alla data d’arrivo), viene mostrato un messaggio di errore.                                                                                                        |
| **Post-condizioni**     | La prenotazione è aggiornata o rimossa dal sistema.                                                                                                                                                                      |

---

### **Use Case 2.3 – Richiesta servizi extra**

| **Elemento**            | **Descrizione**                                                                                                                                                                                    |
| ----------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **Passi Azione**        | 1. Il cliente accede alla sezione “Servizi Extra” della prenotazione.<br>2. Il cliente seleziona uno o più servizi disponibili.<br>3. Il sistema aggiorna la prenotazione con i servizi richiesti. |
| **Attori**              | Cliente                                                                                                                                                                                            |
| **Precondizioni**       | Il cliente ha una prenotazione attiva.                                                                                                                                                             |
| **Scenario principale** | Il cliente aggiunge servizi aggiuntivi (es. colazione, spa, lavanderia).                                                                                                                           |
| **Scenari alternativi** | Alcuni servizi potrebbero non essere disponibili nelle date selezionate.                                                                                                                           |
| **Post-condizioni**     | I servizi extra vengono associati alla prenotazione.                                                                                                                                               |

---

### **Use Case 2.4 – Pagamento online**

| **Elemento**            | **Descrizione**                                                                                                                                                                              |
| ----------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **Passi Azione**        | 1. Il cliente accede alla sezione pagamenti della prenotazione.<br>2. Seleziona il metodo di pagamento (es. carta di credito, PayPal).<br>3. Il sistema processa il pagamento e lo conferma. |
| **Attori**              | Cliente                                                                                                                                                                                      |
| **Precondizioni**       | Il cliente ha una prenotazione valida e accesso al sistema.                                                                                                                                  |
| **Scenario principale** | Il cliente paga online per la prenotazione.                                                                                                                                                  |
| **Scenari alternativi** | Il pagamento può fallire (es. carta rifiutata), viene mostrato un messaggio d’errore.                                                                                                        |
| **Post-condizioni**     | La prenotazione risulta pagata e pronta per il check-in.                                                                                                                                     |

---

# **Use Case: Amministrazione**:

---

### **Use Case 4.1 – Gestione Prenotazioni Clienti**

| **Elemento**            | **Descrizione**                                                                                                                                                                          |
| ----------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **Passi Azione**        | 1. L’amministrazione visualizza le prenotazioni esistenti nel sistema.<br>2. Crea una nuova prenotazione su richiesta del cliente.<br>3. Modifica o cancella una prenotazione esistente. |
| **Attori**              | Amministrazione, Cliente                                                                                                                                                                 |
| **Precondizioni**       | L’amministrazione ha effettuato il login al sistema.                                                                                                                                     |
| **Scenario principale** | L’amministrazione gestisce tutte le operazioni relative alle prenotazioni dei clienti.                                                                                                   |
| **Scenari alternativi** | - Se il cliente cambia data o preferenze, la prenotazione viene aggiornata.<br>- Se la prenotazione viene annullata, l’amministrazione la rimuove.                                       |
| **Post-condizioni**     | Le prenotazioni sono aggiornate nel sistema secondo le richieste del cliente.                                                                                                            |

---

### **Use Case 4.3 – Assegnazione Camere**

| **Elemento**            | **Descrizione**                                                                                                                                                                              |
| ----------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **Passi Azione**        | 1. L’amministrazione verifica la disponibilità delle camere.<br>2. Assegna una camera compatibile con le preferenze del cliente.                                                             |
| **Attori**              | Amministrazione, Cliente                                                                                                                                                                     |
| **Precondizioni**       | Deve essere presente una prenotazione attiva per il cliente.                                                                                                                                 |
| **Scenario principale** | L’amministrazione assegna una camera al cliente sulla base delle disponibilità.                                                                                                              |
| **Scenari alternativi** | - Se la camera preferita non è disponibile, viene assegnata una camera alternativa.<br>- Se non ci sono camere disponibili, l’amministrazione contatta il cliente per soluzioni alternative. |
| **Post-condizioni**     | La camera è assegnata e visibile nel sistema di gestione camere.                                                                                                                             |

---

### **Use Case 4.2 – Gestione Check-in/Check-out**

| **Elemento**            | **Descrizione**                                                                                                                                                                    |
| ----------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **Passi Azione**        | 1. L’amministrazione registra il check-in del cliente all’arrivo.<br>2. Alla partenza, registra il check-out nel sistema.                                                          |
| **Attori**              | Amministrazione, Cliente                                                                                                                                                           |
| **Precondizioni**       | Il cliente deve avere una prenotazione confermata.                                                                                                                                 |
| **Scenario principale** | L’amministrazione gestisce l’ingresso e l’uscita del cliente in struttura.                                                                                                         |
| **Scenari alternativi** | - Se il cliente arriva in anticipo o in ritardo, il sistema gestisce gli orari flessibili.<br>- Se durante il check-out emergono anomalie (es. danni, ritardi), vengono segnalate. |
| **Post-condizioni**     | Il soggiorno del cliente risulta completato nel sistema.                                                                                                                           |

---

### **Use Case 4.4 – Gestione Reclami e Richieste**

| **Elemento**            | **Descrizione**                                                                                                                                                                                      |
| ----------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **Passi Azione**        | 1. L’amministrazione riceve un reclamo o una richiesta dal cliente.<br>2. Registra il reclamo/richiesta nel sistema.<br>3. Smista la segnalazione al reparto competente (es. manutenzione, pulizie). |
| **Attori**              | Amministrazione, Cliente, Amministratore                                                                                                                                                             |
| **Precondizioni**       | Il cliente deve essere registrato o in soggiorno.                                                                                                                                                    |
| **Scenario principale** | L’amministrazione gestisce la segnalazione e la inoltra a chi di competenza.                                                                                                                         |
| **Scenari alternativi** | - Se la segnalazione è urgente, viene contrassegnata con priorità alta.<br>- Se è generica o non urgente, viene messa in coda.                                                                       |
| **Post-condizioni**     | La richiesta è stata presa in carico e smistata correttamente.                                                                                                                                       |

---


### **Use Case 4.5 – Gestione Utenti nel Sistema**

| **Elemento**            | **Descrizione**                                                                                                                                                                                                                                                    |
| ----------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| **Passi Azione**        | 1. L’amministrazione accede al pannello di gestione utenti.<br>2. Crea un nuovo utente (Receptionist, Addetto Pulizie, ecc.) inserendo dati e ruolo.<br>3. Modifica i dati di un utente esistente.<br>4. Disattiva un utente che non deve più accedere al sistema. |
| **Attori**              | Amministrazione                                                                                                                                                                                                                                                    |
| **Precondizioni**       | L’amministrazione ha effettuato il login con credenziali abilitate alla gestione utenti.                                                                                                                                                                           |
| **Scenario principale** | L’amministrazione crea, modifica o disattiva utenti del sistema.                                                                                                                                                                                                   |
| **Scenari alternativi** | - Se l’utente da modificare non esiste, il sistema mostra un messaggio di errore.<br>- Se si tenta di disattivare un utente già disattivato, il sistema blocca l’azione.                                                                                           |
| **Post-condizioni**     | Il sistema rispecchia correttamente la situazione aggiornata degli utenti.                                                                                                                                                                                         |

---

### **Use Case 4.6 – Monitoraggio Generale delle Attività**

| **Elemento**            | **Descrizione**                                                                                                                                                                                          |
| ----------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **Passi Azione**        | 1. L’amministrazione accede alla dashboard di monitoraggio.<br>2. Seleziona il tipo di statistica da visualizzare (occupazione, servizi, feedback).<br>3. Il sistema mostra dati aggregati e aggiornati. |
| **Attori**              | Amministrazione                                                                                                                                                                                          |
| **Precondizioni**       | L’amministrazione ha accesso ai moduli statistici e di reportistica.                                                                                                                                     |
| **Scenario principale** | L’amministrazione consulta dati relativi alle attività della struttura.                                                                                                                                  |
| **Scenari alternativi** | - Se i dati non sono disponibili per il periodo selezionato, il sistema avvisa l’utente.<br>- Se viene selezionato un filtro errato, viene chiesto di correggerlo.                                       |
| **Post-condizioni**     | L’amministrazione ha una visione completa e aggiornata dell’andamento della struttura.                                                                                                                   |

---


# **Casi d'uso: Servizio alberghiero**
---

### **Use Case 5.1 – Visualizza camere da pulire**

| **Elemento**            | **Descrizione**                                                                                                                                    |
| ----------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------- |
| **Passi Azione**        | 1. L’operatore del Servizio alberghiero accede alla sezione camere.<br>2. Il sistema mostra l’elenco delle camere da pulire ordinate per priorità. |
| **Attori**              | Servizio alberghiero                                                                                                                               |
| **Precondizioni**       | L’operatore è autenticato nel sistema.                                                                                                             |
| **Scenario principale** | L’operatore visualizza le camere che richiedono pulizia.                                                                                           |
| **Scenari alternativi** | Se non ci sono camere da pulire, viene mostrato un messaggio di avviso.                                                                            |
| **Post-condizioni**     | L’operatore ha le informazioni necessarie per iniziare la pulizia.                                                                                 |

---

### **Use Case 5.2 – Segnala completamento pulizia**

| **Elemento**            | **Descrizione**                                                                                                       |
| ----------------------- | --------------------------------------------------------------------------------------------------------------------- |
| **Passi Azione**        | 1. L’operatore seleziona la camera pulita dall’elenco.<br>2. Il sistema aggiorna lo stato della camera come “pulita”. |
| **Attori**              | Servizio alberghiero                                                                                                  |
| **Precondizioni**       | La camera deve essere presente nell’elenco delle camere da pulire.                                                    |
| **Scenario principale** | L’operatore aggiorna lo stato della camera dopo aver completato la pulizia.                                           |
| **Scenari alternativi** | Se la camera è già segnata come pulita, viene mostrato un avviso.                                                     |
| **Post-condizioni**     | Lo stato della camera è aggiornato e visibile ad altri ruoli.                                      |

---

### **Use Case 5.3 – Gestione Ordini dei Clienti**

| **Elemento**            | **Descrizione**                                                                                                                                                                                                                                                                           |
| ----------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **Passi Azione**        | 1. Il sistema manda una notifica dell'ordine effettuato.<br>2. Il Servizio alberghiero visualizza i dettagli dell'ordine (pietanza, n. camera, ecc.).<br>3. L'ordine viene preparato, consegnato e contrassegnato "completo".                                                             |
| **Attori**              | Servizio alberghiero, Cliente                                                                                                                                                                                                                                                             |
| **Precondizioni**       | Un Cliente effettua una richiesta di servizio ristorazione.                                                                                                                                                                                                                               |
| **Scenario principale** | Il Servizio alberghiero riceve e gestisce l’ordine del Cliente in camera o in sala.                                                                                                                                                                                                       |
| **Scenari alternativi** | - 3.1a L'ordine non soddisfa il cliente (pietanza sbagliata, mal preparata, ecc.)<br>- 3.2a L'ordine deve essere ripreparato, modificato o annullato su richiesta del cliente.<br>- 3.1b Il Cliente non è presente in camera.<br>- 3.2b L'ordine viene contrassegnato "consegna fallita". |
| **Post-condizioni**     | L'ordine del Cliente è stato gestito.                                                                                                                                                                                                                                                     |

---

### **Use Case 5.4 – Aggiornamento Menù**

| **Elemento**            | **Descrizione**                                                                                                                                     |
| ----------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------- |
| **Passi Azione**        | 1. Il Servizio alberghiero accede all'area "Modifica Menù".<br>2. Effettua le modifiche desiderate.<br>3. Pubblica la versione aggiornata del Menù. |
| **Attori**              | Servizio alberghiero                                                                                                                                |
| **Precondizioni**       | L’utente è autorizzato alla modifica del menù.                                                                                                      |
| **Scenario principale** | Il Servizio alberghiero aggiorna e pubblica il menù del giorno.                                                                                     |
| **Scenari alternativi** | - 2.1 Le modifiche non sono valide (dati insufficienti o con errori).<br>- 2.2 Il sistema mostra un errore e ritorna al form di modifica.           |
| **Post-condizioni**     | Il menù del giorno è stato aggiornato e pubblicato.                                                                                                 |

---



Suddividere gli use case complessi (organizzatore del corso) e fare activity diagram