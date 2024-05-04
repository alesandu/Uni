//esercizio in cui viene creato un database rappresentato da un array di interi
//più threads tentato di accedere all'array per leggere il suo contenuto
//un semaforo regola l'accesso al database facendo accedere 1 thread alla volta

#include <stdio.h> 
#include <pthread.h> 
#include <semaphore.h> 
#include <unistd.h> 
  
sem_t semaforo; //semaforo
int database [] = {1,2,3,4,5,6,7,8,9,10}; //database contenente 10 numeri

//funzione che effettua una wait sul semaforo e una volta entrato leggete i numeri presenti nel database e li stampa uno alla volta
//dopo che ha stampato i numeri presenti nel db rilascia il semaforo 
void leggiDatabase(int threadNum){

    //acquisisco il semaforo e dormo per 2 secondi
    sem_wait(&semaforo);
    printf("[Thread %d] - Entrato, leggo il db...\n",threadNum);
    sleep(2);


    //stampo i numeri nel database
    for(int i = 0; i < 10; i++){
        printf("[Thread %d] - numero letto %d\n",threadNum,database[i]);
    }

    //rilascio il semaforo
    printf("[Thread %d] - Finito, rilascio\n",threadNum);
    sem_post(&semaforo);
}


int main(){

    //inizializzo il semaforo, il terzo parametro è il valore iniziale del semaforo ovvero quanti threads possono accedere al semaforo contemporaneamente
    //il secondo parametro è 0 perchè il semaforo è condiviso tra threads, in caso fosse condiviso tra processi il valore sarebbe 1
    sem_init(&semaforo, 0, 1); 

    pthread_t t1,t2,t3;

    //creo e avvio i threads
    pthread_create(&t1,NULL,leggiDatabase,1);
    pthread_create(&t2,NULL,leggiDatabase,2);
    pthread_create(&t3,NULL,leggiDatabase,3);

    //join dei threads
    pthread_join(t1,NULL);
    pthread_join(t2,NULL);
    pthread_join(t3,NULL);

    sem_destroy(&semaforo); //distruggo il semaforo

    return 0;
}