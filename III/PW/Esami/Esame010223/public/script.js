"use strict"
window.onload = async function(){

    async function fetchDati() {
        try {
            const res = await fetch("http://localhost:3000/citations")
            const data = await res.json()
            return data
        } catch (error) {
            console.log("Error fetching data, ", error)
            return null
        }
    }

    function mostra(el){
        const p = document.getElementById("frase")
        p.innerHTML = "<p>"+el.frase+"</p><p>"+el.valore+"</p>"
        p.style.display = "block"        
    }
    
    async function aggiungi(){
        const dati = await fetchDati()
        const div = document.getElementById("blocchi")
        dati.forEach((el, idx) => {
            const blocco = document.createElement("div")
            blocco.className = "blocco"
            blocco.innerHTML = "<b> "+ el.ID+ "</b>"
            blocco.addEventListener("click",() => mostra(el))
            div.appendChild(blocco)
        });
    }

    aggiungi()
}