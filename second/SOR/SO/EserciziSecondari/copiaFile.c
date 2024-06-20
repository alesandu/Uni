//Programma che effettua la copia di un file in un altro file

#include <sys/types.h>  // Include i file header necessari
#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>

#define BUF_SIZE 4096   // Dimensione del buffer: 4096 byte
#define OUTPUT_MODE 0700    // Bit di protezione per il file di output

#define TRUE 1

int main(int argc, char *argv[]) {
    
    int in_fd, out_fd;   // File descriptor per i file di input e output

    int rd_count, wt_count;  // Contatori per la lettura e scrittura, rd_count per i caratteri letti, wt_count per i caratteri scritti
    char buffer[BUF_SIZE];   // Buffer per la lettura e scrittura dei dati, di dimensione BUF_SIZE (quindi  puo' contenere BUF_SIZE caratteri)

    //Controllo del numero di argomenti
    //come argomenti vengono passati il file di input e il file di output
    if (argc != 3) {
        // Stampa un messaggio di errore se il numero di argomenti non è corretto
        fprintf(stderr, "Errore di sintassi. Uso: %s input_file_path output_file_path\n", argv[0]);
        exit(1);
    }

    // Apertura del file di input
    in_fd = open(argv[1], O_RDONLY);  // Apre il file di origine (quello che si vuole copiare), argv[1] è il primo argomento passato al programma nella riga di comando

    if (in_fd < 0) exit(2);  // controllo se il file è stato aperto correttamente, se non è stato aperto correttamente esce

    // Creazione del file di output
    out_fd = creat(argv[2], OUTPUT_MODE);  // Crea il file di destinazione


    if (out_fd < 0) exit(3);  // Controllo se il file è stato creato correttamente, se non è stato creato correttamente esce

    // Ciclo di copia
    while (TRUE) {

        rd_count = read(in_fd, buffer, BUF_SIZE);  // Legge un blocco di dati, i parametri sono il file descriptor, il buffer in cui scrivere i dati e la dimensione del buffer (ovvero il numero di byte da leggere)
        
        if (rd_count <= 0) break;  // Se rd_count <= 0, la lettura è terminata perché non ci sono più dati da leggere oppure c'è stato un errore

        printf("Ho letto questa parte del file: %s\n", buffer);

        wt_count = write(out_fd, buffer, rd_count);  // Scrive i dati nel file di output, i parametri sono il file descriptor del file di output, il buffer da cui leggere i dati e il numero di byte da scrivere

        if (wt_count <= 0) exit(4);  // Se wt_count <= 0, c'è stato un errore nella scrittura
    }

    // Chiusura dei file di input e output
    close(in_fd);
    close(out_fd);

    printf("Ho completato la copia!\n");

    if (rd_count == 0) exit(0);  // rd_count sarebbero i caratteri letti nell'ultima lettura, se è 0 la copia è andata a buon fine altrimenti c'è stato un errore
    else exit(5);  // Errore sull’ultima lettura
}
