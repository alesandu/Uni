"use strict"
"use strict"
window.onload = async function(){

    async function fetchDati() {
        try {
            const res = await fetch("http://localhost:3000/list")
            const data = await res.json()
            return data
        } catch (error) {
            console.log("Error fetching data, ", error)
            return null
        }
    }

    async function fetchDesc(id) {
        try {
            const res = await fetch("http://localhost:3000/pics/" + id)
            const data = await res.json()
            return data
        } catch (error) {
            console.log("Error fetching data, ", error)
            return null
        }
    }

    async function mostra(id){
        const dati = await fetchDesc(id)
        const overlay = document.getElementById("overlay")
        overlay.innerHTML = dati.desc        
        overlay.style.display = "flex"
    }

    async function carica(n){
        const dati = await fetchDati()
        const ul = document.getElementById("lista")
        ul.innerHTML = ""
        for (let index = 0; index < n; index++) {
            const li = document.createElement("li")
            const a = document.createElement("a")
            a.innerHTML = dati[index]
            li.appendChild(a)
            li.addEventListener("click", () => mostra(dati[index]))
            ul.appendChild(li)
        }
        const botton = document.getElementById("butto")
        botton.addEventListener("click", () => carica(n+4)) 
    }

    async function main(n) {
        const dati = await fetchDati()
        const ul = document.getElementById("lista")
        for (let index = 0; index < n; index++) {
            const li = document.createElement("li")
            const a = document.createElement("a")
            a.innerHTML = dati[index]
            li.appendChild(a)
            li.addEventListener("click", () => mostra(dati[index]))
            ul.appendChild(li)
        }
        const botton = document.getElementById("butto")
        botton.addEventListener("click", () => carica(n+4))        
    }

    main(4)
}

