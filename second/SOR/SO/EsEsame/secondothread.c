#include <fcntl.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

pthread_mutex_t mutex;
pthread_cond_t cond;
pthread_t pri, sec, che;
int mod = 0;

void *pris() {
  pthread_mutex_lock(&mutex);
  int fd = open("input.txt", O_WRONLY);
  int a = rand() % 4;
  printf("%d ", a);
  write(fd, &a, sizeof(a));
  close(fd);
  mod = 1;
  pthread_cond_signal(&cond);
  pthread_mutex_unlock(&mutex);
}

void *secs() {
  pthread_mutex_lock(&mutex);
  int fd = open("input.txt", O_WRONLY);
  int a = rand() % 50;
  printf("%d ", a);
  lseek(fd, 0, SEEK_END);
  write(fd, &a, sizeof(a));
  close(fd);
  mod = 1;
  pthread_cond_signal(&cond);
  pthread_mutex_unlock(&mutex);
}

void *ches() {
  pthread_mutex_lock(&mutex);
  while (!mod) {
    pthread_cond_wait(&cond, &mutex);
  }
  int fd = open("input.txt", O_RDONLY);
  int a, b;
  read(fd, &a, sizeof(a));
  lseek(fd, -sizeof(b), SEEK_END);
  read(fd, &b, sizeof(b));
  int n = a + b;
  printf("\n%d", n);
  pthread_mutex_unlock(&mutex);
}

int main() {
  srand(time(NULL));

  pthread_mutex_init(&mutex, NULL);
  pthread_cond_init(&cond, NULL);

  pthread_create(&pri, NULL, pris, NULL);
  pthread_create(&sec, NULL, secs, NULL);
  pthread_create(&che, NULL, ches, NULL);

  pthread_join(pri, NULL);
  pthread_join(sec, NULL);
  pthread_join(che, NULL);

  pthread_mutex_destroy(&mutex);
  pthread_cond_destroy(&cond);

  return 0;
}
