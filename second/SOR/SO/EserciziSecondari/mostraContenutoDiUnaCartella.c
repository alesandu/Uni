//Programma che si occupa di stampare le informazioni di tutti i file presenti in una cartella
//la cartealla da esaminare viene passata come argomento al programma

#include <stdio.h>
#include <dirent.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>
#include <pwd.h>
#include <grp.h>
#include <time.h>

//funzione che stampa le informazioni di un file, venendo passati il nome della cartella e il nome del file
void printFileInfo(const char* dirName, const char* fileName) {
    
    //struct stat è utilizzata per ottenere informazioni su un file o directory
    struct stat fileInfo;
    char filePath[1024];

    printf("--------------------\n");

    //stampo il nome della cartella e il nome del file (che sono stati passati come argomenti alla funzione)
    printf("Cartella: %s\n", dirName);
    printf("Nome file: %s\n", fileName);

    //in filepath scrivo la stringa formata dal nome della cartella e dal nome del file
    //uso il formato CATELLA/NOMEFILE
    snprintf(filePath, sizeof(filePath), "%s/%s", dirName, fileName);

    printf("Percorso completo del file: %s\n", filePath);

    //controllo eventuali errori nel recupero delle informazioni del file
    //utilizzando stat(filePath, &fileInfo) inserisco le informazioni del file nella struttura fileInfo
    //filepath è una stringa che contiene effettivamente il percorso del file formato CARTELLA/NOMEFILE
    if (stat(filePath, &fileInfo) < 0) {
        perror("Errore nel recupero delle informazioni del file");
        return;
    }

    //recupero la dimensione del file e la inserisco in una variabile chiamata fileSize
    off_t fileSize = fileInfo.st_size;

    printf("Dimensione file: %llu\n", fileSize);

    // Permessi del file
    printf((S_ISDIR(fileInfo.st_mode)) ? "d" : "-"); //tipo di file
    printf((fileInfo.st_mode & S_IRUSR) ? "r" : "-"); //permessi utente da questa riga in poi
    printf((fileInfo.st_mode & S_IWUSR) ? "w" : "-"); //IWUSR sarebbe I Write User
    printf((fileInfo.st_mode & S_IXUSR) ? "x" : "-"); //IXUSR sarebbe I eXecute User
    printf((fileInfo.st_mode & S_IRGRP) ? "r" : "-"); //IRGRP sarebbe I Read GRouP
    printf((fileInfo.st_mode & S_IWGRP) ? "w" : "-"); //IWGRP sarebbe I Write GRouP
    printf((fileInfo.st_mode & S_IXGRP) ? "x" : "-"); //IXGRP sarebbe I eXecute GRouP
    printf((fileInfo.st_mode & S_IROTH) ? "r" : "-"); //IROTH sarebbe I Read OTHers
    printf((fileInfo.st_mode & S_IWOTH) ? "w" : "-"); //IWOTH sarebbe I Write OTHers
    printf((fileInfo.st_mode & S_IXOTH) ? "x" : "-"); //IXOTH sarebbe I eXecute OTHers
    printf("\n");
    
    //Ottengo Proprietario e gruppo
    struct passwd *pw = getpwuid(fileInfo.st_uid);
    struct group  *gr = getgrgid(fileInfo.st_gid);
    printf("Proprietario: %s\n", pw->pw_name);
    printf("Gruppo: %s\n", gr->gr_name);

    //Ottengo Data di ultima modifica
    char dateStr[128];
    strftime(dateStr, sizeof(dateStr), "%b %d %H:%M", localtime(&fileInfo.st_mtime));
    printf("Data ultima modifica: %s\n", dateStr);

}


int main(int argc, char *argv[]) {

    DIR *dirp; //Puntatore alla directory, DIR è un tipo di dato definito in dirent.h

    struct dirent *dirent; //puntatore che mi servirà per leggere i singoli file dentro la directory

    //controllo che sia stato passato un argomento al programma da riga di comando
    if (argc != 2) {
        fprintf(stderr, "Errore: manca inputfile. Uso: %s <directory>\n", argv[0]);
        return 1;
    }

    //apro la directory e controllo che sia stata aperta correttamente
    dirp = opendir(argv[1]);
    if (dirp == NULL) {
        perror("Errore nell'apertura della directory");
        return 1;
    }

    //leggo tutti gli elementi dentro una directory
    //dirent conterra tutte le info del file che sto leggendo
    while ((dirent = readdir(dirp)) != NULL) {

        //dirent ora contiene le informazioni del file corrente e passo queste informazioni alla funzione printFileInfo
        printFileInfo(argv[1], dirent->d_name);
    }

    //chiudo la directory
    closedir(dirp);
    return 0;
}

