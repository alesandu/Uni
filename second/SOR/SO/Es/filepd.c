#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

int main(int argv, char **argc) {

  srand(time(NULL));

  int file =
      open("output.txt", O_CREAT | O_WRONLY | O_TRUNC, S_IRUSR | S_IWUSR);

  int pid1, pid2;
  pid1 = fork();

  if (pid1 == 0) {
    for (int i = 0; i < 10; i++) {
      int n = rand() % 10;
      while (n % 2 != 0) {
        n = rand() % 10;
      }
      write(file, &n, sizeof(int));
      printf("p: %d\n", n);
    }
    close(file);
    exit(1);
  } else if (pid1 > 0) {
    pid2 = fork();
    if (pid2 == 0) {
      for (int i = 0; i < 10; i++) {
        int n = rand() % 10;
        while (n % 2 == 0) {
          n = rand() % 10;
        }
        write(file, &n, sizeof(int));
        printf("d: %d\n", n);
      }
      close(file);
      exit(1);
    } else if (pid2 > 0) {
      waitpid(pid1, NULL, 0);
      waitpid(pid2, NULL, 0);
      close(file);
      int file = open("output.txt", O_RDONLY);
      int n = 0;
      while (read(file, &n, sizeof(int)) > 0) {
        printf("%d\n", n);
      }
      close(file);
    }
  }
  return 0;
}
