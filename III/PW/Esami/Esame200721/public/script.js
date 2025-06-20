"use strict"

async function fetchPeople() {
    try {
        const res = await fetch("http://localhost:3000/data")
        const data = await res.json()
        return data
    } catch (error) {
        console.log("Error fetching data, ", error)
        return null
    }
}

async function main() {
    const colori = await fetchPeople()
    const main = document.getElementById("colori")
    colori.forEach(el => {
        const section = document.createElement("section")
        section.innerHTML = "<h3>"+ el.titolo + "</h3>"
        section.style.backgroundColor = el.colore
        main.appendChild(section)
    });
    const botton = document.getElementById("bottone")
    const over = document.getElementById("over")
    botton.addEventListener("click", () => {
        over.style.display = "none"
    })        
}

main()
