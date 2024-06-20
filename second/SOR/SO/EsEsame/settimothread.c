#include <fcntl.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

pthread_t uno, due, tre;
pthread_cond_t cond;
pthread_mutex_t mutex;
int mod = 0;

void *unos() {
  pthread_mutex_lock(&mutex);
  int fd = open("input.txt", O_WRONLY);
  int n = rand() % 6;
  printf("1: %d\n", n);
  lseek(fd, 0, SEEK_SET);
  write(fd, &n, sizeof(n));
  close(fd);
  mod += 1;
  pthread_cond_signal(&cond);
  pthread_mutex_unlock(&mutex);
  pthread_exit(0);
}

void *dues() {
  pthread_mutex_lock(&mutex);
  int fd = open("input.txt", O_WRONLY);
  int n = rand() % 6;
  printf("2: %d\n", n);
  lseek(fd, -sizeof(n), SEEK_END);
  write(fd, &n, sizeof(n));
  close(fd);
  mod += 1;
  pthread_cond_signal(&cond);
  pthread_mutex_unlock(&mutex);
  pthread_exit(0);
}

void *tres() {
  pthread_mutex_lock(&mutex);
  while (mod < 2) {
    pthread_cond_wait(&cond, &mutex);
  }
  int fd = open("input.txt", O_RDONLY);
  int a, b, sum = 0;
  lseek(fd, 0, SEEK_SET);
  read(fd, &a, sizeof(a));
  lseek(fd, -sizeof(b), SEEK_END);
  read(fd, &b, sizeof(b));
  sum = a + b;
  printf("sum: %d", sum);
  close(fd);
  pthread_mutex_unlock(&mutex);
  pthread_exit(0);
}

int main() {
  srand(time(NULL));
  pthread_mutex_init(&mutex, NULL);
  pthread_cond_init(&cond, NULL);

  pthread_create(&uno, NULL, unos, NULL);
  pthread_create(&due, NULL, dues, NULL);
  pthread_create(&tre, NULL, tres, NULL);

  pthread_join(uno, NULL);
  pthread_join(due, NULL);
  pthread_join(tre, NULL);

  pthread_cond_destroy(&cond);
  pthread_mutex_destroy(&mutex);
  return 0;
}
