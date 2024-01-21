#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

pthread_t cre, max, med;
pthread_mutex_t mutex;
int buffer[10];
int modificato = 1;

void *creone() {
  pthread_mutex_lock(&mutex);
  for (int i = 0; i < 10; i++) {
    buffer[i] = rand() % 101;
    printf("%d ", buffer[i]);
  }
  printf("\n");
  modificato = 0;
  pthread_mutex_unlock(&mutex);
}

void *maxone() {
  while (1) {
    pthread_mutex_lock(&mutex);
    if (!modificato) {
      int m = 0;
      for (int i = 0; i < 10; i++) {
        if (buffer[i] > m) {
          m = buffer[i];
        }
      }
      printf("m: %d\n", m);
      pthread_mutex_unlock(&mutex);
      break;
    }
    pthread_mutex_unlock(&mutex);
    sleep(rand() % 2);
  }
}

void *medone() {
  while (1) {
    pthread_mutex_lock(&mutex);
    if (!modificato) {
      int p = 1000;
      for (int i = 0; i < 10; i++) {
        if (buffer[i] < p) {
          p = buffer[i];
        }
      }
      printf("p: %d\n", p);

      pthread_mutex_unlock(&mutex);
      break;
    }
    pthread_mutex_unlock(&mutex);
    sleep(rand() % 2);
  }
}

int main(int argv, char **argc) {

  srand(time(NULL));

  pthread_mutex_init(&mutex, NULL);

  pthread_create(&cre, NULL, creone, NULL);
  pthread_create(&max, NULL, maxone, NULL);
  pthread_create(&med, NULL, medone, NULL);

  pthread_join(cre, NULL);
  pthread_join(max, NULL);
  pthread_join(med, NULL);

  pthread_mutex_destroy(&mutex);

  fflush(stdout);
  return 0;
}
