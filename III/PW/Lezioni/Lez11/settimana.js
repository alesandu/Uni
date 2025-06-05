let giorno = [
    "lunedì",
    "martedì",
    "mercoledì",
    "giovedì",
    "venerdì",
    "sabato",
    "domenica",
];

console.log(giorno); // stampa l'array giorno
/*In america la settimana parte da domenica. 
Trasformiamo l'array mettendo "domenica" all'inizio*/
giorno.unshift(giorno.pop()); // sposta l'ultimo elemento in prima posizione
console.log(giorno); // stampa l'array giorno

giorno.forEach((el) => console.log(el)); // stampa l'array giorno con forEach
for (let i = 0; i < giorno.length; i++) {
    console.log(giorno[i]); // stampa l'array giorno con for
}
for (let i = 0; i < giorno.length; i++) {
    console.log(giorno.i); // stampa l'array giorno con for
}

giorno.indexOf("martedì"); // restituisce l'indice di martedì

giorno.join(":-)"); // restituisce una stringa con gli elementi dell'array separati da ":-"