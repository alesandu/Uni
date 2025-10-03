#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

int main() {

  int fd1[2];
  int fd2[2];

  int pid1, pid2;

  if ((pipe(fd1) < 0) || (pipe(fd2) < 0)) {
    return -1;
  }

  int file = open("input.txt", O_RDONLY);

  pid1 = fork();
  if (pid1 == 0) {
    close(fd1[0]);
    close(fd2[1]);
    close(fd2[0]);
    int n;
    read(file, &n, sizeof(n));
    printf("1: %d\n", n);
    write(fd1[1], &n, sizeof(n));
    close(fd1[1]);
  } else if (pid1 > 0) {
    pid2 = fork();
    if (pid2 == 0) {
      close(fd1[0]);
      close(fd1[1]);
      close(fd2[0]);
      int n;
      lseek(file, -sizeof(n), SEEK_END);
      read(file, &n, sizeof(n));
      printf("2: %d\n", n);
      write(fd2[1], &n, sizeof(n));
      close(fd2[1]);
    } else if (pid2 > 0) {
      close(fd1[1]);
      close(fd2[1]);
      int a, b;
      read(fd1[0], &a, sizeof(a));
      read(fd2[0], &b, sizeof(b));
      int sum = a + b;
      printf("\n%d", sum);
      close(fd1[0]);
      close(fd2[0]);
    }
  }
  close(file);
  return 0;
}
