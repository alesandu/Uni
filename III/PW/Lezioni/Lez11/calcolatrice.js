"use script";
// Calcolatrice
let calc = {
    n1: 0,
    n2: 0,
    insert(p1, p2) {
        this.n1 = p1;
        this.n2 = p2;
    },
    add() {
        return this.n1 + this.n2;
    },
    sub() {
        return this.n1 - this.n2;
    },
    mul() {
        return this.n1 * this.n2;
    },
    div() {
        if (this.n2 === 0) {
            throw new Error ("Divisione per zero non fattibile");
        }
        return this.n1 / this.n2;
    },
};