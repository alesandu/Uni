#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

/*Scrivere un programma C che segue le seguenti specifiche.
Il processo eseguito, inizialmente crea un buffer come array di 11 numeri
interi, inizializzati a zero. In seguito genera tre thread utilizzando le
librerie POSIX secondo le seguenti specifiche:
•  Il primo thread in un ciclo
infinito sceglie casualmente una cella del buffer e vi scrive il numero +1,
qualsiasi sia il valore presente nella cella.
•  Il secondo thread in un ciclo
infinito sceglie casualmente una cella del buffer e vi scrive il numero -1,
qualsiasi sia il valore presente nella cella.
•  Il terzo thread in un ciclo
infinito controlla se tutte le celle del buffer sono state inizializzate ad un
valore diverso da 0. In caso positivo, determina se il numero di celle
contenenti un valore pari a +1 è maggiore di quelle con -1 e termina tutti e tre
i thread.
•  Mentre un thread ha accesso al buffer, nessun altro thread deve
accedervi.
•  Una volta che un thread ha acceduto in lettura o scrittura al
buffer, deve attendere un numero di secondi random tra 0 e 3
*/

pthread_mutex_t mutex;
pthread_cond_t cond;
pthread_t pos, neg, control;
int buffer[11] = {0};
int modified = 0;

void *posone() {
  while (1) {
    pthread_mutex_lock(&mutex);

    int i = rand() % 11;
    buffer[i] = 1;
    printf("Imposto %d 1\n", i);

    modified = 1;

    pthread_cond_signal(&cond);
    pthread_mutex_unlock(&mutex);
    sleep(rand() % 4);
  }
}

void *negone() {
  while (1) {
    pthread_mutex_lock(&mutex);

    int i = rand() % 11;
    buffer[i] = -1;
    printf("Imposto %d -1\n", i);

    modified = 1;

    pthread_cond_signal(&cond);
    pthread_mutex_unlock(&mutex);
    sleep(rand() % 4);
  }
}

void *controlone() {
  while (1) {
    pthread_mutex_lock(&mutex);

    while (!modified) {
      pthread_cond_wait(&cond, &mutex);
    }

    int c = 0;
    for (int i = 0; i < 11; i++) {
      if (buffer[i] != 0) {
        c++;
      }
      printf("%d : %d\n", i, buffer[i]);
    }
    printf("\n");
    if (c == 11) {
      pthread_cancel(pos);
      pthread_cancel(neg);
      pthread_exit(0);
    }

    modified = 0;
    pthread_mutex_unlock(&mutex);
    sleep(rand() % 4);
  }
}

int main(int argc, char **argv) {

  srand(time(NULL));

  pthread_mutex_init(&mutex, NULL);
  pthread_cond_init(&cond, NULL);

  pthread_create(&pos, NULL, posone, NULL);
  pthread_create(&neg, NULL, negone, NULL);
  pthread_create(&control, NULL, controlone, NULL);

  pthread_join(control, NULL);
  pthread_join(pos, NULL);
  pthread_join(neg, NULL);

  pthread_cond_destroy(&cond);
  pthread_mutex_destroy(&mutex);

  return 0;
}
