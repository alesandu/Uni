/*Modificare l'array per:
1. Convertire i nomi in maiuscolo
2. Aggiungere "Dr. " prima del nome
3. Calcolare chi ha il nome più lungo*/

let names = ['mario', 'giovanna', 'pippo'];

names=names.map((item) => "Dr. " + item.toUpperCase()); // Convertire i nomi in maiuscolo e aggiungere "Dr. "
console.log(names); // stampa l'array names
names.reduce((acc, item, idx, arr) => item.length > arr[acc].length ? idx : acc, 0); // Calcolare chi ha il nome più lungo
console.log(names); // stampa l'array names