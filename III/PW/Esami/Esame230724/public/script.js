"use strict"
window.onload = async function(){

    async function fetchArti() {
        try {
            const res = await fetch("http://localhost:3000/articoli")
            const data = await res.json()
            return data
        } catch (error) {
            console.log("Error fetching data, ", error)
            return null
        }
    }

    async function fetchAut() {
        try {
            const res = await fetch("http://localhost:3000/autori")
            const data = await res.json()
            return data
        } catch (error) {
            console.log("Error fetching data, ", error)
            return null
        }
    }

    function specifica(el){
        const text = "Titolo: " + el.titolo + "\n" + "Autore: " + el.autore + "\n"+ "Conteunto: Lorem ipsum" 
        alert(text)

    }

    async function mostra(){
        const dati = await fetchAut()
        const ul = document.createElement("ul")
        const testo = document.getElementById("testo")
        dati.forEach((element, idx) => {
            const li = document.createElement("li")
            li.innerHTML = element
            ul.appendChild(li)
        });
        testo.innerHTML = "<h2>Elenco autori</h2>"
        testo.appendChild(ul)
        const div = document.getElementById("overlay")
        div.style.display = "flex"
    }

    function leva(){
        const div = document.getElementById("overlay")
        div.style.display = "none"
    }

    async function add() {
        const articoli = document.getElementById("articoli")
        const dati = await fetchArti()
        dati.forEach(el => {
            const button = document.createElement("button")
            button.innerHTML = "Mostra dettagli"
            button.addEventListener("click",() => specifica(el))
            const div = document.createElement("div")
            div.classList = "articolo"
            div.innerHTML = "<h3>"+ el.titolo +"</h3>"+ "<p><b>Autore</b>:" + el.autore + "</p><p>" + el.contenuto + "/<p>"
            div.appendChild(button)
            articoli.appendChild(div)
        });
        const modale = document.getElementById("mostra")
        modale.addEventListener("click",() => mostra())
        const x = document.getElementById("x")
        x.addEventListener("click",() => leva())
            
    }

    add()
    
}