#include <pthread.h>
#include <semaphore.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

sem_t mutex;
sem_t arr;
pthread_t writers;
pthread_t readers[5];
int buffer[10] = {0};
int r = 0;

void *lett(void *args) {
  for (int i = 0; i < 10; i++) {
    sem_wait(&mutex);
    r++;
    if (r == 1) {
      sem_wait(&arr);
    }
    sem_post(&mutex);

    int n = rand() % 10;
    int a = buffer[n];

    sem_wait(&mutex);
    r--;
    if (r == 0) {
      sem_post(&arr);
    }
    sem_post(&mutex);
    printf("l(%d): %d", n, a);
    printf("\n");
    sleep(1);
  }
  pthread_exit(NULL);
}

void *scrit(void *args) {
  for (int i = 0; i < 10; i++) {
    int n = rand() % 10;
    int a = rand() % 6;
    sem_wait(&arr);
    buffer[n] = a;
    for (int i = 0; i < 10; i++) {
      printf("%d", buffer[i]);
    }
    printf("\n");
    sem_post(&arr);
    sleep(1);
  }
  pthread_exit(NULL);
}

int main() {
  srand(time(NULL));

  sem_init(&mutex, 0, 1);
  sem_init(&arr, 0, 1);

  for (int i = 0; i < 5; i++) {
    pthread_create(&readers[i], NULL, lett, NULL);
  }
  pthread_create(&writers, NULL, scrit, NULL);

  for (int i = 0; i < 5; i++) {
    pthread_join(readers[i], NULL);
  }
  pthread_join(writers, NULL);

  sem_destroy(&mutex);
  sem_destroy(&arr);
  return 0;
}
