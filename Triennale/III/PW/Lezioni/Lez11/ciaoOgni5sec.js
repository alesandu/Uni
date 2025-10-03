function print_hello() {
    console.log("Ciao");
}
let i = setInterval(print_hello, 5000); // ogni 5 secondi
// setInterval è una funzione che permette di eseguire una funzione a intervalli regolari
i
// clearInterval(i); // per fermare l'intervallo