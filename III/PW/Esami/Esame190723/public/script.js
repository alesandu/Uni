"use strict"

async function fetchBlocco() {
    try {
        const res = await fetch("http://localhost:3000/prod")
        const data = await res.json()
        return data
    } catch (error) {
        console.log("Error fetching data, ", error)
        return null
    }
}

async function riduzione(el, idx) {
    const div = document.getElementById(idx)
    el.numero = el.numero - 1;
    div.innerHTML = "Prodotto: " + el.prodotto + "<br>" + "Numero: " + el.numero;
}

async function addBlocchi() {
    const blocchi = await fetchBlocco()
    const main = document.getElementById("princp")
    blocchi.forEach((element, idx) => {
        const div = document.createElement("div")
        div.innerHTML = "Prodotto: " + element.prodotto + "<br>" + "Numero: " + element.numero;
        div.id = idx
        div.classList.add(element.colore, "box")  
        div.addEventListener("click", () => riduzione(element, idx))
        main.appendChild(div)
    });
}

setTimeout(addBlocchi,5000)
