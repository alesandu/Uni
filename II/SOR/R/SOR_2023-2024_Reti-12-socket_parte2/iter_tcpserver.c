
#include <sys/types.h> /* per compatibilita' con vari sistemi */
#include <stdlib.h> /* per EXIT_SUCCESS*/
#include <sys/socket.h> /* per socket, struct sockaddr, sockaddr_storage, bind, listen */
#include <netinet/in.h> /* per IPPROTO_TCP, htonl, htons, INET_ADDRSTRLEN */
#include <string.h>  /* per memset */
#include <stdio.h> /* per perror */
#include <arpa/inet.h> /* per inet_ntop */
#include <stddef.h>
#include <unistd.h> /* per close, fork */
#include <errno.h> /* per errno e EINTR */

/* Definisco alcune costanti come macro */
#define PORT 5000
#define BACKLOG 10 
#define BUFFER_SIZE 100

/* prototipo di una funzione ausiliaria per l'invio di n byte */
ssize_t sendn(int fd, const void * buffer, size_t n);

int main(int argc, char *argv[]) {
    /* 
        Dichiaro due variabili, rispettivamente per la "listening socket" 
        (usata per ricevere richieste di connessione) e per la "connected socket"
        (creata quando viene accettata una richiesta di connessione pendente)
     */
    int listenfd, connfd;

    /*
        Dichiaro due variabili che conterrano l'indirizzo del server e del client.
        Sto usando struct sockaddr_storage che è definita in modo che:
        * sia piu' grande di qualunque struct sockaddr_XX (per esempio, per IPv4 e IPv6)
        * sia allineata in modo tale che un puntatore puo' essere convertito (casted) in un
          puntatore di tipo struct sockaddr_xx

        Ci permette di scrivere un codice compatibile sia con IPv4 sia con IPv6

        Si noti che il field ss_family e' corrisponde al campo sin_family di struct sockaddr_in
    */
    struct sockaddr_storage myaddr, clientaddr;
    socklen_t myadd_effectivelen;
    socklen_t clientaddrlen;

    char ipstr[INET6_ADDRSTRLEN]; // usiamo una costante per la lunghezza di un indirizzo IPv6 (altrimenti, INET_ADDRSTRLEN per IPv4)

    char buffer[BUFFER_SIZE];
    /*
        Si sarebbe potuto usare getaddrinfo per ottere un indirizzo IP appropriato: si noti che questa
        e' una estensione POSIX che deve essere esplicitamente abilitata in gcc.
        Per semplicita', cabliamo l'uso di 0.0.0.0 IPv4
    */
    memset(&myaddr, 0, sizeof(myaddr));
    struct sockaddr_in *addrptr = (struct sockaddr_in *)&myaddr;
    addrptr->sin_family = AF_INET;
    inet_pton(addrptr->sin_family, "0.0.0.0", &addrptr->sin_addr);
    addrptr->sin_port = htons(PORT); /* Si noti la conversion dalla rappresentazione dell'host a quella della rete! */
    myadd_effectivelen = sizeof(struct sockaddr_in);

    /*
        Creo la socket. Si noti l'uso di SOCK_STREAM come tipo di socket,
        ovvero la semantica della comunicazione, nello specifico comunicazione
        orienta alla connessione, basata sul trafsferimento affidabile, bidirezionale
        di un flusso di byte.

        Si noti che in questo momento la socket non ha ancora un indirizzo e non puo'
        inviare ne' ricevere dati.

        Nota: restituisce il file descriptor della socket, oppure -1 in caso di errore (dettagliato in errno)
    */
    if ((listenfd = socket(myaddr.ss_family, SOCK_STREAM, IPPROTO_TCP)) == -1) {
        perror("socket");
        exit(EXIT_FAILURE);
    }

    /* 
        Uso bind per associare alla socket un indirizzo locale (indirizzo IP e numero di porta).
    */
    if (bind(listenfd, (struct sockaddr *) &myaddr, myadd_effectivelen) == -1) { /* restituisce 0 in caso di successo; -1 altrimenti */
        perror("bind");
        close(listenfd);
        exit(EXIT_FAILURE);
    }

    /*
        Preparo la socket per accettare connessioni. Il secondo argomento indica
        (correntemente) il numero di connessioni completamente stabilite per possono
        essere accodate in attesa di essere accettate dal server.

        Il valore del secondo argomento viene troncato se superiore a un valore massimo
        di sistema, che si può trovare qui: /proc/sys/net/core/somaxconn
    */
    if (listen(listenfd, BACKLOG) == -1) {
        perror("listen");
        close(listenfd);
        exit(EXIT_FAILURE);
    }

    while (1) {
        memset(&clientaddr, 0, sizeof(clientaddr));
        clientaddrlen = sizeof(clientaddr);

        /*
            Accetta una connessione in attesa sulla listening socket. Restituisce il file descriptor
            della socket connessa (al peer remoto), oppure -1 in caso di errore (dettagliato nella varibile errno)    
        */
        if ((connfd = accept(listenfd, (struct sockaddr *)&clientaddr, &clientaddrlen)) == -1) { /* accept si blocca finche' non c'è una connessione */
            perror("accept");
            continue; /* passa avanti e tenta di accettare una nuova connessione */
        }

        /* Converto l'indirizzo del client in stringa. Nota: non dovrebbero esserci errori*/
        if (clientaddr.ss_family == AF_INET) {
            inet_ntop(clientaddr.ss_family, &((struct sockaddr_in *)&clientaddr)->sin_addr, ipstr, sizeof(ipstr));
        } else {
            inet_ntop(clientaddr.ss_family, &((struct sockaddr_in6 *)&clientaddr)->sin6_addr, ipstr, sizeof(ipstr));
        }
 
        /* Stampa di debug. Si noti la conversione del numero di porta!!! */
        fprintf(stderr, "Accepted connection from: network address = %s ; port = %d\n",
            ipstr,
            ntohs(clientaddr.ss_family == AF_INET ? ((struct sockaddr_in *)&clientaddr)->sin_port : ((struct sockaddr_in6 *)&clientaddr)->sin6_port));

        /* Echo server che reinvia i dati ricevuti! */

        while (1) {
            ssize_t nread = recv(connfd, &buffer, sizeof(buffer), 0);
            if (nread == -1) { /* restituisce il numero di byte letti oppure -1 in caso di errore*/
                perror("recv");
                break; /* esce dal ciclo infinito, in modo da raggiungere l'istruzione che chiude la connessione*/
            }

            if (nread == 0) {
                break; // il client ha chiuso la connessione, quindi il sever esce dal ciclo e eventualmente chiudera' il suo lato
            }


  
            ssize_t nwritten = sendn(connfd, &buffer, nread); /* NOTA: e' la nostra funzione ausiliaria! */
            if (nwritten == -1) { /* restituisce il numero di byte scritti oppure -1 in caso di errore*/
                perror("send");
            }
        }

        /* Stampa di debug. Si noti la conversione del numero di porta!!! */
        fprintf(stderr, "Closing connection to: network address = %s ; port = %d\n",
            ipstr,
            htons(clientaddr.ss_family == AF_INET ? ((struct sockaddr_in *)&clientaddr)->sin_port : ((struct sockaddr_in6 *)&clientaddr)->sin6_port));


        if (close(connfd) == -1) {
            perror("close");
        }
    }

    /* In realta' questa porizione di codice non viene mai eseguita! Per terminare il programma, bisogna killarlo */
    if (close(listenfd) == -1) {
        perror("listenfd close: ");
        exit(EXIT_FAILURE);
    }
    
    return EXIT_SUCCESS;
}

/*
    Funzione ausiliaria per trasmettere tutti e n i byte
*/
ssize_t sendn(int fd, const void * buffer, size_t n) {
    size_t toSend = n;
    const void * sendBuffer = buffer;
    while (toSend > 0) {
        ssize_t nwritten = send(fd, sendBuffer, toSend, 0);
        if (nwritten == -1) { /* restituisce il numero di byte scritti oppure -1 in caso di errore*/
            /* NOTA: non gestendo segnali, EINTR non dovrebbe mai essere restituito */
            if (errno == EINTR) continue; /* non e' un errore, continua con la scrittura */
            
            return -1;
        }

        toSend -= nwritten;
        sendBuffer = (void*)((char *)sendBuffer + nwritten);
    }

    return n;
}