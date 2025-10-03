"use strict"

async function fetchDati() {
    try {
        const res = await fetch("http://localhost:3000/task")
        const data = await res.json()
        return data
    } catch (error) {
        console.log("Error fetching data, ", error)
        return null
    }
}

async function cambio(element){
    const res = await fetch("http://localhost:3000/task/complete/"+element.id, {
        method: "POST"
    })
    const dati = await fetchDati()
    const sez = document.getElementById("sezione")
    sez.innerHTML = ""
    dati.forEach(element => {
        const att = document.createElement("div")
        const butt = document.createElement("button")
        butt.innerHTML = "Completa"
        if(element.completed == true){
            butt.className = "completa"
        }
        butt.addEventListener("click", () => cambio(element))
        att.className = "attivita"
        att.innerHTML = element.text
        att.appendChild(butt)
        sez.append(att)
    });

}

async function addBlocchi() {
    const dati = await fetchDati()
    const sez = document.getElementById("sezione")
    dati.forEach(element => {
        const att = document.createElement("div")
        const butt = document.createElement("button")
        butt.innerHTML = "Completa"
        if(element.completed == true){
            butt.className = "completa"
        }
        butt.addEventListener("click", () => cambio(element))
        att.className = "attivita"
        att.innerHTML = element.text
        att.appendChild(butt)
        sez.append(att)
    });
}

addBlocchi()
