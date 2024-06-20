//Programma in cui c'è un buffer di 11 celle inizializzato a 0.
//Il primo thread aggiunge 1 in una posizione casuale del buffer e dorme per un tempo casuale.
//Il secondo thread aggiunge -1 in una posizione casuale del buffer e dorme per un tempo casuale.
//Il terzo thread controlla se il buffer è stato inizializzato e controlla se ci sono più 1 o -1 nel buffer.

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <pthread.h>
#include <time.h>
#include <signal.h>

//inizializzo mutex e condition
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t condition = PTHREAD_COND_INITIALIZER;

int buffer[11]; //buffer di 11 celle inizializzato a 0
pthread_t threadsArray[3]; //array di thread

#define TRUE 1
#define FALSE 1
#define bufferSize 11

//funzione che stampa il contenuto del buffer
void* printBuffer(){

    for(int i = 0; i < bufferSize; i++){
        printf("Position %d -> %d\n",i,buffer[i]);
    }
    printf("\n");
}

//handler del segnale SIGINT
//questa funzione viene eseguita quando il processo riceve un segnale SIGINT
void* signalHandler(int signo){
    if(signo == SIGINT){
        printf("Ricevuto segnale SIGINT\n");

        //termino il thread che ha ricevuto il segnale
        pthread_exit(0);
    }
}

//funzione che aggiunge il numero 1 in una posizione casuale del buffer
void* aggiungiUnoPositivo(){

    //in caso di ricezione del segnale SIGINT eseguo la funzione signalHandler
    signal(SIGINT,signalHandler);

    while(TRUE){

        //blocco il mutex
        pthread_mutex_lock(&mutex);

        //estraggo il numero di cella casuale
        int randomCell = random() % 12;

        //inserisco 1 nella cella
        buffer[randomCell] = 1;

        //estraggo un numero casuale di secondi da dormire
        int sleepTime = random() % 4;

        //print
        printf("Sono il thread positivo e ho aggiunto 1 in posizione %d, dormo per %d secondi\n", randomCell,sleepTime);
        printBuffer();

        //rilascio il mutex e invio un segnale
        pthread_cond_signal(&condition);
        pthread_mutex_unlock(&mutex);

        //dormo
        sleep(sleepTime);
    }
}

//funzione che aggiunge il numero -1 in una posizione casuale del buffer
void* aggiungiUnoNegativo(){

    signal(SIGINT,signalHandler);

    while(TRUE){

        pthread_mutex_lock(&mutex);

        //estraggo il numero di cella casuale
        int randomCell = random() % 12;

        //inserisco 1 nella cella
        buffer[randomCell] = -1;

        //estraggo un numero casuale di secondi da dormire
        int sleepTime = random() % 4;

        //print
        printf("Sono il thread negativo e ho aggiunto -1 in posizione %d, dormo per %d secondi\n", randomCell,sleepTime);
        printBuffer();

        //rilascio il mutex e invio un segnale
        pthread_cond_signal(&condition);
        pthread_mutex_unlock(&mutex);

        //dormo
        sleep(sleepTime);
    }
}

//funzione che controlla se il buffer è stato inizializzato ovvero se tutte le celle sono diverse da 0
//restiuisce 1 se il buffer è stato inizializzato, 0 altrimenti
int checkBufferInit(){
    for(int i = 0; i < bufferSize; i++){
        if(buffer[i] == 0){
            return 0;
        }
    }
    return 1;
}

//funzione che controlla che il buffer sia stato inizializzato e che controlla se ci sono più 1 o -1, verrà eseguita dal terzo thread
void controllaBufferEConta(){

    while(TRUE){

        pthread_mutex_lock(&mutex); //blocco il mutex

        //controllo se il mutex ha qualche 0 eseguendo la funzione checkBufferInit
        while(checkBufferInit() == 0){

            //aspetto perchè il buffer non è stato inizializzato (ho ricevuto 0 dalla funzione checkBufferInit)
            pthread_cond_wait(&condition,&mutex);
        }

        //controllo se ci sono più 1 oppure -1
        int positiveCounter = 0;
        int negativeCounter = 0;

        for(int i = 0; i < bufferSize; i++){

            if(buffer[i]==1){
                positiveCounter++;
            }else{
                negativeCounter++;
            }
        }

        //stampo il risultato
        printf("Ci sono %d numeri positivi\n",positiveCounter);
        printf("Ci sono %d numeri negativi\n",negativeCounter);
        if(positiveCounter > negativeCounter){
            printf("Le celle con un numero positivo sono maggiore di quelle con un numero negativo\n");
        }
        
        //Killo i threads inviando un segnale SIGINT
        //ATTENZIONE LA FUNZIONE KILL SERVE PER INVIERE UN SEGNALE
        pthread_kill(threadsArray[1],SIGINT);
        pthread_kill(threadsArray[2],SIGINT);
        
        //autokill
        pthread_exit(0);

    }
}

int main(){

    //inizializzo rand
    srand(time(NULL));

    //creo i thread
    pthread_create(&threadsArray[0],NULL,&controllaBufferEConta,NULL);
    pthread_create(&threadsArray[1],NULL,&aggiungiUnoPositivo,NULL);
    pthread_create(&threadsArray[2],NULL,&aggiungiUnoNegativo,NULL);

    //join
    pthread_join(threadsArray[0],NULL);
    pthread_join(threadsArray[1],NULL);
    pthread_join(threadsArray[2],NULL);

    //distruggo il mutex
    pthread_mutex_destroy(&mutex);

    return 0;

}