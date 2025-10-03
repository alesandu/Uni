"use strict"
window.onload = async function(){

    async function fetchDati() {
        try {
            const res = await fetch("http://localhost:3000/items")
            const data = await res.json()
            return data
        } catch (error) {
            console.log("Error fetching data, ", error)
            return null
        }
    }

    async function fetchTrue() {
        try {
            const res = await fetch("http://localhost:3000/items-complete")
            const data = await res.json()
            return data
        } catch (error) {
            console.log("Error fetching data, ", error)
            return null
        }
    }

    async function boxmostra(){
        const main = document.querySelector("main")
        main.innerHTML = ""
        const dati = await fetchDati()
        dati.forEach(element => {
            const div = document.createElement("div")
            const button = document.createElement("button")
            if(element.completato == true){
                button.innerHTML = "Completed"
                div.style.textDecoration = "line-through"
                button.disabled = true
            }
            else{
                button.innerHTML = "Complete"
            }
            div.className = "box"
            div.innerHTML = element.testo
            div.appendChild(button)
            main.appendChild(div)
        });
    }

    async function boxveroa(){
        const main = document.querySelector("main")
        main.innerHTML = " "
        const dati = await fetchTrue()
        dati.forEach(element => {
            const div = document.createElement("div")
            const button = document.createElement("button")
            button.disabled = true
                button.innerHTML = "Completed"
                div.className = "box"
                div.style.textDecoration = "line-through"
                div.innerHTML = element.testo
                div.appendChild(button)
                main.appendChild(div)
        });
    }

    async function colorea(){
        const body = document.querySelector("body")
        body.className ="chiaro"
    } 


    async function coloreb(){
        const body = document.querySelector("body")
        body.className ="scuro"
    } 

    async function main() {
        const main = document.querySelector("main")
        const dati = await fetchDati()
        dati.forEach(element => {
            const div = document.createElement("div")
            const button = document.createElement("button")
            if(element.completato == true){
                button.innerHTML = "Completed"
                div.style.textDecoration = "line-through"
                button.disabled = true
            }
            else{
                button.innerHTML = "Complete"
            }
            div.className = "box"
            div.innerHTML = element.testo
            div.appendChild(button)
            main.appendChild(div)
        });

        const mostra = document.getElementById("mostra")
        const vero = document.getElementById("vero")
        const chiaro = document.getElementById("chiaro")
        const scuro = document.getElementById("scuro")
        mostra.addEventListener("click", () => boxmostra())
        vero.addEventListener("click", () => boxveroa())
        chiaro.addEventListener("click", () => colorea())
        scuro.addEventListener("click", () => coloreb())
    }

    main()
}

