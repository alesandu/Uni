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

    const dati = await fetchDati()
    dati.forEach(el => {
            console.log(el)
        });

    
    function aggiungi(){
        const main = document.querySelector("main")
        main.innerHTML = ""
        dati.forEach(el => {
            const div = document.createElement("div")
            div.className = "quadrato"
            div.style.top = el.pos_vert+"%"
            div.style.left = el.pos_orizz+"%"
            div.style.backgroundColor = el.colore
            div.addEventListener("click",() => div.remove)
            main.appendChild(div)
        });
    }

    async function disegna() {
        const titolo = document.querySelector("h1")
        titolo.addEventListener("click", () => aggiungi())    
    }

    disegna()
    
}