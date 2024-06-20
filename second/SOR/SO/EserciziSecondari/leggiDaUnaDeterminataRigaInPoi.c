//Questo programma sposta il cursore ad una determinata riga e comincia a leggere il file da quella riga in poi

//Librerie necessarie
#include <sys/types.h>  
#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>

#define BUFFER_SIZE 1 //buffer di dimensione 1 dove appoggiare i dati letti

int main(){

    int line = 3; //voglio cominciare a leggere dalla riga 3 in poi

    char buffer[BUFFER_SIZE]; //creo un buffer di dimensione 1 (dimensione BUFFER_SIZE)

    int fd_in, charCounter, endLineCounter, currentCharCounter, currentChar;

    fd_in = open("input.txt", O_RDONLY);

    endLineCounter = 0;
    charCounter = 0;

    //algoritmo uguale a quello dell'esercizio precedente con il quale contiamo i caratteri letti finchè non arriviamo alla riga desiderata
    while(endLineCounter != (line-1)){

        currentCharCounter = read(fd_in,buffer,BUFFER_SIZE);

        if (buffer[0] == '\n') {
            endLineCounter++;
        }
        charCounter++;
    }

    //mi posiziono alla posizione charCounter (charCounter è il numero di caratteri letti finora)
    lseek(fd_in, charCounter, SEEK_SET);

    while(1){

        //leggo un carattere alla volta e lo stampo
        currentChar = read(fd_in,buffer,BUFFER_SIZE);

        //se currentChar <= 0 significa che ho finito di leggere il file oppure c'è stato un errore
        if(currentChar <= 0){

            break;
        }else{ 

            //stampo il carattere letto (siccome il buffer è di dimensione 1, buffer[0] contiene il carattere letto)
            printf("%c", buffer[0]);
        }
    }
    
}