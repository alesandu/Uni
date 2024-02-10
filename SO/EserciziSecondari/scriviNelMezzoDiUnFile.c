//Questo programma sposta il cursore ad una determinata riga e scrive SONO QUI

#include <sys/types.h>  // Include i file header necessari
#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>

#define BUFFER_SIZE 1 //buffer di dimensione 1 dove appoggiare i dati letti

int main(){

    int line = 3; //numero della linea da quale voglio cominciare a leggere

    char buffer[BUFFER_SIZE];

    //variabili
    //fd: file descriptor del file di input
    //charCounter: contatore dei caratteri letti
    //endLineCounter: contatore dei caratteri \n letti (ovvero delle righe lette)
    //currentCharCounter: contatore dei caratteri letti in una singola lettura
    int fd, charCounter, endLineCounter, currentCharCounter;

    endLineCounter = 0;
    charCounter = 0;
    currentCharCounter = 0;

    fd = open("input.txt", O_RDWR); //apro il file in lettura e scrittura (fd contiene il file descriptor del file di input)

    //come nell'esercizio precedente leggo un carattere alla volta finchè non arrivo alla riga desiderata
    while(endLineCounter != (line-1)){

        //leggo un carattere alla volta
        currentCharCounter = read(fd,buffer,BUFFER_SIZE);

        //se il carattere letto è \n incremento il contatore delle righe signficca che ho terminato di leggere una riga (\n è il carattere di fine riga)
        if (buffer[0] == '\n') {
            endLineCounter++;
        }

        //incremento il contatore dei caratteri letti
        charCounter++;

    }

    printf("Ho letto %d caratteri\n", charCounter);

    //mi posizione alla posizione charCounter (charCounter è il numero di caratteri letti finora)
    lseek(fd, charCounter, SEEK_SET);

    //scrivo SONO QUI nella posizione in cui mi trovo nel file
    write(fd, "SONO QUI", sizeof("SONO QUI"));

    return 0;
}