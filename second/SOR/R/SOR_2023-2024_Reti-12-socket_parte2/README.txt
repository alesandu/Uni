
Programmi:
  * preprocessor.c
    Semplice programma per mostrare il funzionamento del preprocessore
    
  * iter_tcpserver (TCP server iterativo)
    Potete interrogarlo con:

    nc -N localhost 5000

    l'opzione -N fa in modo che il quando incontrato EOF (che si puo' generare con CTRL+D), il client chiuda
    la propria parte della connessione (tramite shutdown), rimanendo in attesa che il server completi di
    scrivere dati e chiuda la propria parte della connessione.

    Ho notato che nc chiude effettivamente la connessione se CTRL+D viene premuto all'inizio del prompt
    (cioe' all'avvio o dopo un a capo). Altrimenti, invia i dati ma occorre premere una seconda volta CTLR+D

    Il nome "server iterativo" e' legato al fatto che il server itera sulle connessioni in attesa,
    accettandola una alla volta. Provate a connettervi due volte contemporaneamente!

  Osservazioni:
    - se dimenticate htons quando settate il numero di porta, vedrete che il server andra' in
      ascolto su una porta diversa
    - se provate a togliere qualche cast, potete vedere il tipo di warning/errori segnalati da gcc

Materiale vario:
  * Materiale su POSIX sockets API
  * Materiale addizionale su ss