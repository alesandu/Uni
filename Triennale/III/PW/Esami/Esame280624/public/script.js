"use strict"
"use strict"
window.onload = async function(){

    async function fetchDati() {
        try {
            const res = await fetch("http://localhost:3000/persone")
            const data = await res.json()
            return data
        } catch (error) {
            console.log("Error fetching data, ", error)
            return null
        }
    }

    function mostra(el){
        const testo = "Nome: " + el.nome + " " + el.cognome + ", Età: " + el.età
        alert(testo)
    }

    function cambio(){
        const body = document.querySelector("body")
        body.style.backgroundColor = "#00796b"
        const a = document.querySelectorAll("a")
        a.forEach(el => el.style.color = "#313131")
    }

    async function main() {
        const dati = await fetchDati()
        const ul = document.createElement("ul")
        dati.forEach(el => {
            const li = document.createElement("li")
            li.innerHTML = el.nome + " " + el.cognome
            li.addEventListener("click", () => mostra(el))
            ul.appendChild(li)
        });
        const prima = document.getElementById("prima")
        prima.appendChild(ul)
        const botton = document.getElementById("butto")
        botton.addEventListener("click", () => cambio())        
    }

    main()
}

