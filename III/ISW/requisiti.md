| Attore                | ID   | Requisiti Funzionali                         | Descrizione                                                                                  |
|-----------------------|------|----------------------------------------------|----------------------------------------------------------------------------------------------|
| Utente non registrato | 1.1  | Visualizzazione offerte e camere disponibili | Può consultare le camere disponibili, i servizi offerti e i prezzi senza registrazione.      |
|                       | 1.2  | Registrazione                                | Può registrarsi inserendo dati personali per diventare un cliente.                          |
| Cliente               | 2.1  | Effettua prenotazione                        | Può prenotare camere selezionando data di arrivo, partenza e tipo di stanza.                |
|                       | 2.2  | Gestione prenotazioni                        | Può visualizzare, modificare o cancellare le prenotazioni effettuate.                       |
|                       | 2.4  | Richiesta servizi extra                      | Può richiedere servizi come colazione in camera, lavanderia, spa, ecc.                      |
|                       | 2.5  | Check-in e Check-out online                  | Può effettuare operazioni di check-in/check-out direttamente dal sistema.                   |
|                       | 2.6  | Pagamento online                             | Può effettuare il pagamento tramite carta di credito o altri metodi.                        |
| Utente registrato     | 3.1  | Effettua accesso                             |                                                                                              |
|                       | 3.2  | Cancellazione account                        |                                                                                              |
|                       | 3.3  | Visualizza camere                            |                                                                                              |
|                       | 3.4  | Aggiornamento dati personali                 | Può aggiornare i propri dati come nome, email, telefono, ecc.                               |
| Receptionist          | 4.1  | Gestione prenotazioni clienti                | Può visualizzare, creare, modificare o cancellare prenotazioni per conto dei clienti.       |
|                       | 4.2  | Gestione check-in/check-out                  | Registra l'ingresso e l'uscita degli ospiti.                                                 |
|                       | 4.3  | Assegnazione camere                          | Assegna camere ai clienti in base a disponibilità e preferenze.                             |
|                       | 4.4  | Gestione reclami e richieste                 | Registra ed eventualmente smista le segnalazioni a chi di competenza.                      |
| Servizio pulizie      | 5.1  | Visualizza camere da pulire                  | Può vedere la lista delle camere da pulire ordinate per urgenza.                            |
|                       | 5.2  | Segnala completamento pulizia                | Può aggiornare lo stato della camera una volta pulita.                                      |
| Servizio ristorazione | 6.1  | Gestione ordini dei clienti                  | Riceve richieste dei clienti per servizio in camera o in sala.                              |
|                       | 6.2  | Menù aggiornabile                            | Può aggiornare il menù giornaliero visibile ai clienti.                                     |
| Amministratore        | 7.1  | Gestione utenti del sistema                  | Può creare, modificare o disattivare utenti (receptionist, pulizie, ecc.).                  |
|                       | 7.2  | Monitoraggio generale attività               | Può consultare statistiche di occupazione, servizi usati, feedback clienti.                 |

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

Il software è progettato per semplificare e automatizzare la gestione operativa di una struttura alberghiera, riducendo il rischio di errori, ottimizzando i processi e migliorando l’esperienza degli ospiti. Il sistema supporta le attività quotidiane del personale, consentendo una gestione integrata delle prenotazioni, del check-in/check-out, della pulizia delle camere, della contabilità e della comunicazione interna.

Gli utenti non registrati possono consultare camere disponibili, offerte e prezzi senza obbligo di registrazione. Se interessati, possono procedere alla registrazione inserendo i propri dati personali per accedere a servizi avanzati.

Gli utenti registrati possono effettuare prenotazioni online selezionando le date, la tipologia di camera e i servizi aggiuntivi desiderati. Le disponibilità vengono aggiornate in tempo reale e ogni prenotazione è automaticamente notificata al personale di ricevimento. Hanno la possibilità di gestire in autonomia le proprie prenotazioni (modifiche, cancellazioni), effettuare il **check-in e check-out online**, aggiornare i propri dati e concludere i pagamenti attraverso metodi elettronici integrati.

Il receptionist ha accesso a funzionalità avanzate per gestire le prenotazioni dei clienti, assegnare le camere in base alla disponibilità, registrare ingressi/uscite e gestire reclami o richieste speciali, smistandoli a chi di competenza.

Il personale addetto alle pulizie riceve notifiche giornaliere sulle camere da sistemare, e può consultare in tempo reale la lista delle camere da riordinare e aggiornare lo stato di pulizia al termine delle attività. 

Anche il servizio di ristorazione può gestire ordini ricevuti dai clienti, aggiornare il menù giornaliero e organizzare le consegne in camera o in sala.

L'amministratore dell’hotel ha accesso a un pannello di controllo che consente di monitorare in tempo reale: 
- l’occupazione delle camere, 
- la produttività del personale, 
- i flussi di cassa 
- le recensioni degli ospiti.
Inoltre può creare, modificare o disattivare utenti appartenenti a diverse categorie (receptionist, pulizie, ristorazione)

Attraverso l’integrazione con sistemi di messaggistica (SMS, email, notifiche app), il software permette una comunicazione fluida con gli ospiti prima, durante e dopo il soggiorno, per inviare promemoria, suggerimenti personalizzati e offerte dedicate.

Tutte le operazioni sono tracciate e accessibili in un unico portale centralizzato, disponibile da desktop e dispositivi mobili. Il sistema è progettato per garantire sicurezza, flessibilità e scalabilità, ed è conforme alle normative vigenti in materia di trattamento dei dati personali, rispettando i requisiti normativi in materia di privacy.


# Glossario

| Termine                     | Definizione                                                                                   |
|----------------------------|-----------------------------------------------------------------------------------------------|
| Utente non registrato      | Visitatori del sito o portale che non hanno ancora creato un account.                        |
| Cliente                    | Utente registrato che può effettuare prenotazioni, pagamenti e accedere a servizi personalizzati. |
| Utente registrato          | Qualsiasi utente che ha effettuato la registrazione e può accedere a funzionalità avanzate.  |
| Receptionist               | Membro dello staff incaricato della gestione delle prenotazioni, del check-in/check-out e del contatto diretto con i clienti. |
| Servizio pulizie           | Personale responsabile dell'ordine e della pulizia delle camere.                             |
| Servizio ristorazione      | Personale addetto alla gestione degli ordini per pasti in camera o in sala.                  |
| Amministratore             | Figura con accesso completo al sistema, responsabile della gestione degli utenti e del monitoraggio generale. |
| Prenotazione               | Operazione con cui un cliente riserva una camera per un determinato periodo.                 |
| Check-in                   | Procedura di registrazione all’arrivo del cliente in struttura.                              |
| Check-out                  | Procedura di uscita del cliente al termine del soggiorno.                                    |
| Pagamento online           | Transazione effettuata tramite sistemi elettronici (carta di credito, PayPal, ecc.).         |
| Notifica                   | Messaggio automatico inviato via email, SMS o app per informare l’utente di un evento o attività. |
| Menù aggiornabile          | Lista dei piatti e delle bevande offerti dalla struttura, modificabile dallo staff.          |
| Reclami e richieste        | Segnalazioni o domande inviate dai clienti relative al loro soggiorno.                       |
| Camera disponibile         | Unità abitativa prenotabile e pronta per l’uso.                                               |
| Stato pulizia              | Indicatore aggiornato sullo stato di ordine e igiene di una camera.                          |
| Sistema                    | L'applicazione software per la gestione completa dell’hotel.                                 |
| Portale                    | Interfaccia web o app tramite cui gli utenti interagiscono con il sistema.                   |
