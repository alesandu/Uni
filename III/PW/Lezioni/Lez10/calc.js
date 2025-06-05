"use strict";
let n3=40;

function somma(a,b) {
    return a+b;
}

function calcolatrice() {
    let n1,n2,ret;
    console.log(n3);
    
    n1=prompt("inserisci il primo numero: ");
    n2=prompt("inserisci il secondo numero: ");
    n1=Number(n1);
    n2=Number(n2);

    ret=somma(n1,n2);
    alert("la somma è: "+ret);
}

console.log(n3);
calcolatrice();