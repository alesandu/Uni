"use strict"
window.onload = async function(){
    
    async function fetchDati() {
        try {
            const res = await fetch("http://localhost:3000/dati")
            const data = await res.json()
            return data
        } catch (error) {
            console.log("Error fetching data, ", error)
            return null
        }
    }

    function mostra(dati){
        const main = document.getElementById("main")
        main.innerHTML = ""
        const lista = document.createElement("ul")
        dati.forEach(element => {
           const punto = document.createElement("li")
           punto.innerHTML = element.prodotto + " " +  element.desc
           lista.appendChild(punto)
        });
        main.appendChild(lista)
    }

    async function addBlocchi() {
        const dati = await fetchDati()
        const main = document.getElementById("main")
        const pre = document.createElement("pre")
        const bott = document.createElement("button")
        bott.innerHTML = "Converti"
        bott.addEventListener("click",() => mostra(dati))
        pre.innerHTML = JSON.stringify(dati, null, 2)
        main.appendChild(pre)
        main.appendChild(bott)
    }

    addBlocchi()
}