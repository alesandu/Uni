#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

int main(int argv, char **argc) {
  srand(time(NULL));

  int fd1[2];
  int fd2[2];

  int pid1 = 0, pid2 = 0;

  if ((pipe(fd1) < 0) || (pipe(fd2) < 0)) {
    return -1;
  }

  pid1 = fork();

  if (pid1 == 0) {
    close(fd1[0]);
    close(fd2[0]);
    close(fd2[1]);
    while (1) {
      int a = rand() % 50;
      if (a % 3 == 0)
        write(fd1[1], &a, sizeof(int));
    }
    close(fd1[1]);

  } else if (pid1 > 0) {
    pid2 = fork();
    if (pid2 == 0) {
      close(fd1[0]);
      close(fd1[1]);
      close(fd2[0]);
      while (1) {
        int a = rand() % 50 + 51;
        if (a % 2 == 0) {
          write(fd2[1], &a, sizeof(int));
        }
      }
      close(fd2[1]);
    } else if (pid2 > 0) {
      int sum = 0, a = 0, b = 0;
      close(fd1[1]);
      close(fd2[1]);
      while (sum < 130) {
        read(fd1[0], &a, sizeof(int));
        read(fd2[0], &b, sizeof(int));
        sum = a + b;
        printf("%d + %d -> %d\n", a, b, sum);
      }
      kill(pid1, SIGKILL);
      kill(pid2, SIGKILL);
      close(fd1[0]);
      close(fd2[0]);
    }
  }
  return 0;
}
