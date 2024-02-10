/*
Elettori che si recano a un seggio. 
	-arrivano in istanti di tempo casuali.
	-possono essere presenti max 4 elettori.
	-un elettore deve attendere sulla porta se sono presenti 4 elettori
	-impiega tempo random per votare
*/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <pthread.h>
#include <time.h>

//MAX ELETTORI contemporaneamente
#define MAX 4

pthread_mutex_t mutex; //MUTEX
pthread_cond_t cond; //CONDIZIONE
int elettoriAttuali = 0; //ELETTORI ATTUALMENTE PRESENTI

//funzione eseguita dai thread
void* routine (void* arg){


    pthread_mutex_lock(&mutex); //LOCK del MUTEX

    //se ci sono già 4 elettori, l'elettore attende
    while(elettoriAttuali == MAX){

        printf("Elettore %d in attesa\n", arg);
        pthread_cond_wait(&cond, &mutex); //attende sulla CONDIZIONE del MUTEX
    }

    //se si trova qui significa che l'elettore può entrare
    //incremento il numero di elettori attuali
    elettoriAttuali++;
    printf("Elettore %d entrato, ci sono %d elettori\n", (int)arg, elettoriAttuali);

    //sblocco il MUTEX
    pthread_mutex_unlock(&mutex);

    printf("Elettore %d sta votando\n", (int)arg);

    //DORME PER UN TEMPO CASUALE mentre vota
    int tempo = rand() % 7 + 1;
    sleep(tempo);

    //blocco il MUTEX
    pthread_mutex_lock(&mutex);

    //esco da seggio quindi decremento il numero di elettori attuali
    elettoriAttuali--;
    printf("Elettore %d uscito, ci sono %d elettori\n", (int)arg, elettoriAttuali);
    pthread_cond_signal(&cond); //comunico agli altri elettori in attesa che io sto uscendo e che possono entrare
    pthread_mutex_unlock(&mutex); //sblocco il MUTEX
}

int main(){

    //inizializzo il MUTEX e la CONDIZIONE
    pthread_mutex_init(&mutex, NULL);
    pthread_cond_init(&cond, NULL);

    //creo 6 thread (dentro un array di thread)
    pthread_t listaThreads[6];

    //li creo e gli assegno la funzione routine da eseguire
    for(int i = 0; i < 6; i++){

        //creo il thread passandogli la funzione routine e l'indice i
        //l'indice i è il numero dell'elettore
        pthread_create(&listaThreads[i], NULL, routine, (void*)i);
    }

    //join
    for(int i = 0; i < 6; i++){

        pthread_join(listaThreads[i], NULL);
    }

    //distruggo il MUTEX e la CONDIZIONE
    pthread_mutex_destroy(&mutex);
    pthread_cond_destroy(&cond);
    return 0;

}