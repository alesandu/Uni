#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

pthread_mutex_t mutex;
pthread_cond_t cond;
pthread_t odd, even, check;
int buffer[] = {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1};
int modified = 0;

void *evenone() {
  while (1) {
    pthread_mutex_lock(&mutex);
    int n = rand() % 89 + 10;
    int i = 1;
    while (i % 2 != 0) {
      i = rand() % 10;
    }
    buffer[i] = n;
    modified = 1;
    printf("p [ ");
    for (i = 0; i < 10; i++) {
      printf("%d ", buffer[i]);
    }
    printf("]\n\n");
    pthread_cond_signal(&cond);
    pthread_mutex_unlock(&mutex);
    sleep(rand() % 4);
  }
}

void *oddone() {
  while (1) {
    pthread_mutex_lock(&mutex);
    int n = rand() % 100 + 100;
    int i = 0;
    while (i % 2 == 0) {
      i = rand() % 10;
    }
    buffer[i] = n;
    modified = 1;
    printf("d [ ");
    for (i = 0; i < 10; i++) {
      printf("%d ", buffer[i]);
    }
    printf("]\n\n");
    pthread_cond_signal(&cond);
    pthread_mutex_unlock(&mutex);
    sleep(rand() % 4);
  }
}

void *checkone() {
  while (1) {
    int c = 0;
    pthread_mutex_lock(&mutex);
    while (!modified) {
      pthread_cond_wait(&cond, &mutex);
    }
    for (int i = 0; i < 10; i++) {
      if (buffer[i] != -1) {
        c++;
      }
    }
    if (c == 10) {
      for (int i = 1; i < 10; i++) {
        buffer[i] = buffer[i] + buffer[i - 1];
      }
      for (int i = 0; i < 10; i++) {
        printf("%d ", buffer[i]);
      }
      pthread_cancel(odd);
      pthread_cancel(even);
      pthread_exit(0);
    }
    modified = 0;
    pthread_mutex_unlock(&mutex);
    sleep(rand() % 4);
  }
}

int main(int argv, char **argc) {
  srand(time(NULL));
  pthread_mutex_init(&mutex, NULL);
  pthread_cond_init(&cond, NULL);

  pthread_create(&odd, NULL, oddone, NULL);
  pthread_create(&even, NULL, evenone, NULL);
  pthread_create(&check, NULL, checkone, NULL);

  pthread_join(odd, NULL);
  pthread_join(even, NULL);
  pthread_join(check, NULL);

  pthread_mutex_destroy(&mutex);
  pthread_cond_destroy(&cond);

  return 0;
}
