//Questo programma si occupa di copiare un file da una determinata riga in poi in un altro file

#include <sys/types.h>
#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>

#define BUFFER_SIZE 1 //dimensione del buffer (1 carattere)

int main(){

    //linea dalla quale voglio cominciare leggere
    int line = 1;

    //buffer sul quale appoggiare i dati mentre li leggo
    char buffer[BUFFER_SIZE];

    //variabili
    //fd_in: file descriptor del file di input
    //fd_out: file descriptor del file di output
    //charCounter: contatore dei caratteri letti
    //endLineCounter: contatore dei caratteri \n letti (ovvero delle righe lette)
    //currentCharCounter: contatore dei caratteri letti in una singola lettura
    //currentChar: carattere letto in una singola lettura
    int fd_in, fd_out, charCounter, endLineCounter, currentCharCounter, currentChar;

    //apro il file in lettura
    fd_in = open("directory/input.txt", O_RDONLY);
    if(fd_in < 0){
        printf("Errore nell'apertura del file\n");
        exit(3);
    }

    endLineCounter = 0;
    charCounter = 0;
 
    //se devo arrivare alla riga numero 3 significa che devo leggere due volte il carattere \n perchè ogni volta che termina una riga c'è un carattere \n (ovvero a capo)
    //finchè non arrivo alla riga desiderata leggo un carattere alla volta
    while(endLineCounter != (line-1)){

        //leggo un carattere
        currentCharCounter = read(fd_in,buffer,BUFFER_SIZE);

        //se il carattere letto è \n incremento il contatore delle righe (significa che ho terminato di leggere una riga)
        if (buffer[0] == '\n') {
            endLineCounter++;
        }

        //incremento il contatore dei caratteri totali letti
        charCounter++;
    }

    //se sono qui significa che ho letto n-1 righe e quindi sono pronto per iniziare a copiare il file
    //ad esempio se line = 3, ho letto 2 righe e quindi sono pronto per iniziare a copiare il file dalla terza riga in poi
    //imposto il puntatore del file alla posizione charCounter (charCounter è il numero di caratteri letti finora)
    lseek(fd_in, charCounter, SEEK_SET);

    //apro il file in scrittura
    fd_out = open("output.txt", O_WRONLY);

    while(1){

        //leggo i caratteri dal file di input (sono posizionato alla metà del file)
        currentChar = read(fd_in,buffer,BUFFER_SIZE);

        if(currentChar <= 0){

            //se non ci sono caratteri oppure è stato generato un errore esco
            break;

        }else{

            //scrivo i caratteri nel file di output
            write(fd_out, buffer, currentChar);

            //controllo errori
            if(write <= 0){
                printf("Errore di scrittura\n");
                exit(4);
            }

        }
    }
    
    //chiudo i file
    close(fd_in);
    close(fd_out);
}