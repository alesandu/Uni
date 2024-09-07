/*
Scrivere un programma C che segue le seguenti specifiche.
Il processo eseguito, inizialmente crea un buffer come array di 11 numeri
interi, inizializzati a zero. In seguito genera 2 processi figli utilizzando le
librerie POSIX secondo le seguenti specifiche:
-   Il primo processo filgio sceglie casualmente una cella del buffer e la invia
al padre. Il padre modifica il buffer in quella posizione inserendo 1. Dopo ogni
scrittura su pipe attende un numero di secondi random tra 0 e 3.
-   Il secondo processo figlio sceglie casualmente una cella del buffer e la
invia al padre. Il padre modifica il buffer in quella posizione inserendo -1.
Dopo ogni scrittura su pipe attende un numero di secondi random tra 0 e 3.
-   Il padre dopo ogni modifica del buffer controlla se ci sono ancora degli 0.
In caso affermativo conta gli 1 e gli -1, manda a video il risultato e invia un
segnale di terminazione ai processi.
*/

#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

int main() {
  int buffer[] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
  int fd1[2];
  int fd2[2];
  int pid1 = 0;
  int pid2 = 0;

  if ((pipe(fd1) < 0) || (pipe(fd2) < 0)) {
    return -1;
  }

  pid1 = fork();

  if (pid1 == 0) {
    srand(getpid());
    close(fd1[0]);
    close(fd2[0]);
    close(fd2[1]);
    while (1) {
      int n = rand() % 12;
      write(fd1[1], &n, sizeof(int));
      sleep(rand() % 4);
    }
    close(fd1[1]);

  } else if (pid1 > 0) {
    pid2 = fork();
    if (pid2 == 0) {
      srand(getpid());
      close(fd2[0]);
      close(fd1[0]);
      close(fd1[1]);
      while (1) {
        int n = rand() % 12;
        write(fd2[1], &n, sizeof(int));
        sleep(rand() % 4);
      }
      close(fd2[1]);

    } else if (pid2 > 0) {
      close(fd1[1]);
      close(fd2[1]);
      int pos1, pos2;
      while (1) {
        read(fd1[0], &pos1, sizeof(int));
        buffer[pos1] = 1;
        read(fd2[0], &pos2, sizeof(int));
        buffer[pos2] = -1;
        int c = 0;
        for (int i = 0; i < 11; i++) {
          printf("%d ", buffer[i]);
          if (buffer[i] != 0) {
            c++;
          }
        }
        printf("\n");
        if (c == 11) {
          int piu = 0, neg = 0;
          for (int i = 0; i < 11; i++) {
            if (buffer[i] == 1) {
              piu++;
            }
            if (buffer[i] == -1) {
              neg++;
            }
          }
          printf("%d, %d", piu, neg);
          kill(pid1, SIGTERM);
          kill(pid2, SIGTERM);
          close(fd1[0]);
          close(fd2[0]);
          return 0;
        }
      }
    }
  }
}
