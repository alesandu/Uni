#include <stdio.h>

#define MY_CONSTANT 1 + 1
#define MY_CONSTANT2 (1 + 1)

int main(int argc, char *argv[]) {
    const int my_constant3 = 1 + 1;

    printf("test 1: %d\n", MY_CONSTANT * 2);  /* stampa 3 */
    printf("test 2: %d\n", MY_CONSTANT2 * 2); /* stampa 4 */
    printf("test 3: %d\n", my_constant3 * 2); /* stampa 4*/
 
    /*
        Si noti che il valore della costante viene (logicamente) calcolato al
        momento dell'assegnazione (riga 7).

        Le direttive #define definiscono delle macro, rispettivamente, MY_CONSTANT
        e MY_CONSTANT2, associandole a delle espressioni. Il preprocessore sostituisce
        le occorrenze di queste macro con l'espressione corrispondente (che puo'
        essere valutata piu' volte e in contesti diversi).

        Il risultato differente della prima printf deriva dal fatto che dopo il
        preprocessamento e' come se avessimo scritto:

        printf("test 1: %d\n", 1 + 1 * 2)

        Nella seconda printf abbiamo invece

        printf("test 1: %d\n", (1 + 1) * 2)

        La direttiva #include, invece, inserisce il contenuto del file indicato.

        Ci sono due sintassi:

        #include <file>

            Cerca il file all'interno di un insieme di cartelle (cui possiamo preprenderne
            nuove usando l'opzione -I di gcc)

        #include "file"
        
            Cerca il file all'interno della cartella dove si trova il file corrente, quindi 
            un insieme di cartelle (cui possiamo prependerne con l'opzione -iquote di gcc),
            e infine nelle stesse cartelle usate dalla sintassi con le parentesi angolari.


        Il comando
        
            gcc -E filesorgente.c

        scrive su standard output il risultato del preprocessamento del file filesorgente.c

    */
    return 0;
}