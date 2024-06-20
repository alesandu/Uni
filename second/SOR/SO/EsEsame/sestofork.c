#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

int main() {

  int fd1[2];
  int fd2[2];

  int pid1, pid2;

  int sum = 0;

  if ((pipe(fd1) < 0) || (pipe(fd2) < 0)) {
    return -1;
  }

  pid1 = fork();
  if (pid1 == 0) {
    srand(time(NULL));
    close(fd1[0]);
    close(fd2[0]);
    close(fd2[1]);
    while (1) {
      int n = rand() % 100;
      write(fd1[1], &n, sizeof(n));
    }
    close(fd1[1]);
  } else {
    pid2 = fork();
    if (pid2 == 0) {
      srand(getpid());
      close(fd1[0]);
      close(fd1[1]);
      close(fd2[0]);
      while (1) {
        int n = rand() % 100;
        write(fd2[1], &n, sizeof(n));
      }
      close(fd2[1]);
    } else {
      close(fd1[1]);
      close(fd2[1]);
      while (sum < 190) {
        int a, b;
        read(fd1[0], &a, sizeof(a));
        read(fd2[0], &b, sizeof(b));
        sum = a + b;
        printf("%d + %d: %d\n", a, b, sum);
      }
      close(fd1[0]);
      close(fd2[0]);
    }
  }
  return 0;
}
