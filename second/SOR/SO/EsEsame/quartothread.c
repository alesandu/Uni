#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

pthread_mutex_t mutex;
pthread_cond_t cond;
pthread_t pre1, pre2, pre3;
int sum, a, b;
int mod = 0;

void *pres1() {
  while (1) {
    pthread_mutex_lock(&mutex);
    a = rand() % 100;
    while (a % 2 != 0) {
      a = rand() % 100;
    }
    mod += 5;
    printf("1: %d\n", a);
    pthread_cond_signal(&cond);
    pthread_mutex_unlock(&mutex);
    sleep(rand() % 4);
  }
}

void *pres2() {

  while (1) {
    pthread_mutex_lock(&mutex);
    b = rand() % 100;
    while (b % 2 == 0) {
      b = rand() % 100;
    }
    mod += 5;
    printf("2: %d\n", b);
    pthread_cond_signal(&cond);
    pthread_mutex_unlock(&mutex);
    sleep(rand() % 4);
  }
}

void *pres3() {

  while (1) {
    pthread_mutex_lock(&mutex);
    while (mod < 10) {
      pthread_cond_wait(&cond, &mutex);
    }
    mod = 0;
    printf("check\n");
    sum = a + b;
    if (sum >= 130) {
      printf("3: %d", sum);
      pthread_cancel(pre1);
      pthread_cancel(pre2);
      pthread_exit(0);
    } else {
      a = 0;
      b = 0;
      sum = 0;
      pthread_mutex_unlock(&mutex);
      sleep(rand() % 2);
    }
  }
}

int main() {
  srand(time(NULL));

  pthread_mutex_init(&mutex, NULL);
  pthread_cond_init(&cond, NULL);

  pthread_create(&pre1, NULL, pres1, NULL);
  pthread_create(&pre2, NULL, pres2, NULL);
  pthread_create(&pre3, NULL, pres3, NULL);

  pthread_join(pre1, NULL);
  pthread_join(pre2, NULL);
  pthread_join(pre3, NULL);

  pthread_mutex_destroy(&mutex);
  pthread_cond_destroy(&cond);

  return 0;
}
